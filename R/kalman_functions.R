#' WRTDS-Kalman
#'
#' This function uses an autoregressive model to produce more accurate
#' estimates of concentration and flux
#'
#' This function takes an existing eList
#' Including the estimated model (the surfaces object in the eList)
#' And produces the daily WRTDSKalman estimates of concentration and flux
#' These generated estimates are called GenConc and GenFlux
#'
#' @rdname wrtdsK
#' @export
#' @param eList named list with the INFO, Daily, and Sample dataframes and surfaces matrix
#' @param rho numeric the lag one autocorrelation. Default is 0.9.
#' @param niter number of iterations. Default is 200.
#' @param verbose logical specifying whether or not to display progress message
#' @param seed integer value. Defaults to NA, which will not change the current seed.
#' Setting the seed to any given value can be used to create repeatable output.
#' @examples
#' eList <- Choptank_eList
#' eList <- WRTDSKalman(eList, niter = 10)
#' summary(eList$Daily)
#'
#' #All flux values in AnnualResults are expressed as a rate in kg/day
#' AnnualResults <- setupYears(eList$Daily)
#' head(AnnualResults)
WRTDSKalman <- function(
    eList,
    rho = 0.90,
    niter = 200,
    seed = NA,
    verbose = TRUE
) {
  if (!is.egret(eList)) {
    stop("Please check eList argument")
  }
  
  # Need these columns to be integers:
  int_cols <- c("Julian", "Month", "Day", "MonthSeq", "waterYear")
  int_indexs <- which(names(eList$Daily) %in% int_cols)
  
  eList$Daily[, int_indexs] <- sapply(eList$Daily[, int_indexs], as.integer)
  
  if (!"surfaces" %in% names(eList)) {
    eList$surfaces <- estSurfaces(eList)
  }
  
  if (!is.na(seed)) {
    set.seed(seed)
  }
  
  # this part is to set up the array of runs of missing values
  localEList <- cleanUp(eList, seed = seed)
  localDaily <- populateDailySamp(localEList)
  
  if (sum(is.na(localDaily$trueConc)) == 0) {
    stop(
      "There are known concentration values for every day in the Daily data frame."
    )
  }
  
  numDays <- length(localDaily$Date)
  numDaysP <- numDays + 1
  # set up DailyGen which will hold the daily generated flux values
  # for all days and all iterations
  DailyGen <- rep(0, numDays * niter)
  dim(DailyGen) <- c(numDays, niter)
  # x is a vector of the standardized residuals for each day
  # most of the elements of x will be NA but those from sampled days will have values
  x <- localDaily$stdResid
  # xP is x that has been padded with a 0 at the start and a 0 at the end
  # thus it is a vector that always starts and ends with non-missing values
  xP <- c(0, x, 0)
  zz <- rle(is.na(xP))
  # zz$lengths is a vector of run lengths
  # zz$values is a vector of the values: TRUE means it is a run of missings,
  # FALSE means it is a run of non-missing values
  nRuns <- length(zz$lengths)
  zends <- cumsum(zz$lengths)
  nRunsM <- nRuns - 1
  zstarts <- c(0, zends[1:nRunsM])
  # doGap is the indexs of the runs that are missing values (it is just the even integers)
  doGap <- seq(2, nRunsM, 2)
  # numGap is the number of groups of missing values to be filled in
  numGap <- length(doGap)
  # precompute Cholesky factor shared by every call of genmissing_fast()
  Lfull <- precompute_L(rho, numDays + 2)
  
  # now we are ready to do the iterations to generate the series
  if (verbose) {
    cat("% complete:\n")
  }
  printUpdate <- unique(floor(seq(1, niter, niter / 100)))
  endOfLine <- seq(10, 100, 10)
  seeds <- sample(1:5000, niter)
  
  for (iter in 1:niter) {
    if (iter %in% printUpdate & verbose) {
      cat(floor(iter * 100 / niter), "\t")
      if (floor(iter * 100 / niter) %in% endOfLine) cat("\n")
    }
    localEList <- cleanUp(eList, seed = seeds[iter])
    # this next step adds a trueConc column to Daily, and it is NA if there is no sample value
    # it also adds the stdResid column to Daily
    localDaily <- populateDailySamp(localEList)
    x <- localDaily$stdResid
    # xxP is x that has been padded with a 0 at the start and a 0 at the end
    # thus it is a vector that always starts and ends with non-missing values
    xxP <- c(0, x, 0)
    
    # now we are going to loop through all the gaps that need to be filled in
    for (i in 1:numGap) {
      iGap <- doGap[i]
      startFill <- zstarts[iGap]
      endFill <- zends[iGap] + 1
      nFill <- zz$lengths[iGap] + 2
      # genmissing_fast() uses Lfull created above
      xxP[startFill:endFill] <- genmissing_fast(xxP[startFill], xxP[endFill], rho, nFill, Lfull)
    }
    
    # now we need to strip out the padded days
    xResid <- xxP[2:numDaysP]
    xConc <- exp((xResid * localDaily$SE) + localDaily$yHat)
    DailyGen[, iter] <- xConc * localDaily$Q * 86.4
  }

  # now we take means over all the iterations
  Daily <- eList$Daily
  Daily$GenFlux <- rowMeans(DailyGen, na.rm = TRUE)
  Daily$GenConc <- Daily$GenFlux / (Daily$Q * 86.4)
  attr(Daily, "niter") <- niter
  attr(Daily, "rho") <- rho
  
  eList$Daily <- Daily
  
  return(eList)
}

#' cleanUp eList
#'
#' Takes an eList as the input. If there are duplicated dates in the Sample data frame,
#' will randomly select one value for that date.  If there are censored values
#' in the data set they will be replaced by random censored values.  If there are no
#' days with duplicate samples and no censored valued then the eList returned by the function will be identical to the eList that is passed to it.
#'
#' This function is run before each iteration of generating a random sequence in the
#' \code{\link{WRTDSKalman}} function
#'
#' @param eList named list with the INFO, Daily, and Sample dataframes and surfaces matrix.
#' @param seed integer value. Defaults to NA, which will not change the current seed.
#' Setting the seed to any given value can be used to create repeatable output.
#' @export
#' @return eList with duplicated dates in the Sample data frame randomly sampled and censored values are replaced by random values.
#' @examples
#' eList <- Choptank_eList
#'
#' eList <- cleanUp(eList)
#'
cleanUp <- function(eList, seed = NA) {
  Sample <- randomSubset(eList$Sample, "Julian", seed = seed)
  eListClean <- as.egret(eList$INFO, eList$Daily, Sample, eList$surfaces)
  eListClean <- makeAugmentedSample(eListClean)
  Sample <- eListClean$Sample
  Sample$Uncen <- 1
  Sample$ConcLow <- Sample$rObserved
  Sample$ConcHigh <- Sample$rObserved
  Sample$ConcAve <- Sample$rObserved
  eListClean <- as.egret(eList$INFO, eList$Daily, Sample, eList$surfaces)
  return(eListClean)
}


#' randomSubset
#'
#' Calculates a random subset of the data based on repeated values from
#' a specified column.
#'
#' @export
#' @param df data frame. Must include a column named by the argument colName.
#' @param colName column name to check for duplicates
#' @param seed integer value. Defaults to NA, which will not change the current seed.
#' Setting the seed to any given value can be used to create repeatable output.
#' @examples
#' df <- data.frame(Julian = c(1,2,2,3,4,4,4,6),
#'                  y = 1:8)
#' df
#' df_random <- randomSubset(df, "Julian")
#' df_random
randomSubset <- function(df, colName, seed = NA) {
  dupIndex <- unique(c(
    which(duplicated(df[[colName]], fromLast = FALSE)),
    which(duplicated(df[[colName]], fromLast = TRUE))
  ))
  
  if (length(dupIndex) == 0) {
    return(df)
  }
  
  dupIndex <- dupIndex[order(dupIndex)]
  
  unique_groups <- unique(df[[colName]][dupIndex])
  
  if (!is.na(seed)) {
    set.seed(seed)
  }
  
  sliceIndex <- rep(NA, length(unique_groups))
  for (i in seq_along(unique_groups)) {
    sliceIndex[i] <- sample(which(df[[colName]] == unique_groups[i]), size = 1)
  }
  
  dfDuplicates <- df[sliceIndex, ]
  dfNoDuplicates <- df[-dupIndex, ]
  
  subDF <- rbind(dfNoDuplicates, dfDuplicates)
  subDF <- subDF[order(subDF[[colName]]), ]
  
  return(subDF)
}

#' Merge concentration to Daily
#'
#' Used for the WRTDS Kalman set of functions, this function
#' merges the ConcAve into the Daily data frame, renaming
#' it "trueConc", then calculates the "trueFlux", and "stdResid".
#'
#' @param eList named list with the INFO, Daily, and Sample dataframes
#' and surfaces matrix
#'
#' @export
#' @examples
#' eList <- Choptank_eList
#' Daily2 <- populateDailySamp(eList)
#'
populateDailySamp <- function(eList) {
  localSample <- eList$Sample
  localDaily <- eList$Daily
  
  localSample <- localSample[, c("Julian", "ConcAve")]
  names(localSample) <- c("Julian", "trueConc")
  
  retDaily <- merge(localDaily, localSample, by = "Julian", all.x = TRUE)
  
  retDaily$trueFlux <- retDaily$trueConc * retDaily$Q * 86.4
  retDaily$stdResid <- (log(retDaily$trueConc) - retDaily$yHat) / retDaily$SE
  
  return(retDaily)
}


#' genmissing
#'
#' Generates a lag one auto-regressive time series, where the first and last
#' values are fixed.  Marginal expected value is zero and variance is one.
#' Generated values have a normal conditional distribution.
#'
#' @author Tim Cohn
#'
#' @param X1 value before the gap
#' @param XN value after the gap
#' @param rho the lag one autocorrelation
#' @param N the length of the sequence including X1 and XN. It
#' is two more than the gap length
#' @export
#' @return numeric vector of length N, conditioned on the
#' first value (X1) and last value (XN) with the specified lag one autocorrelation
#' in the limit (where N is large) the values are normal with mean 0 and variance 1
#'
genmissing <- function(X1, XN, rho, N) {
  # this code was done by Tim Cohn
  # @param X1 the value before the gap
  # @param XN  value after the gap
  # @param rho the lag one autocorrelation
  # @param N  the length of the sequence including X1 and XN
  # it is two more than the gap length
  C <- t(chol(
    rho^abs(outer(1:N, 1:N, "-"))[c(1, N, 2:(N - 1)), c(1, N, 2:(N - 1))]
  ))
  
  (C %*%
      c(
        MASS::ginv(C[1:2, 1:2]) %*%
          c(X1, XN),
        stats::rnorm(N - 2)
      ))[c(1, 3:N, 2)]
}


#' genmissing_fast
#'
#' Generates a lag one auto-regressive time series, where the first and last
#' values are fixed.  Marginal expected value is zero and variance is one.
#' Generated values have a normal conditional distribution. This version replaces
#' genmissing and is much faster for long sample gaps.
#'
#'
#' @param X1 value before the gap
#' @param XN value after the gap
#' @param rho the lag one autocorrelation
#' @param N the length of the sequence including X1 and XN. It
#' is two more than the gap length
#' @param Lfull Cholesky factor created with precompute_L
#' @export
#' @return numeric vector of length N, conditioned on the
#' first value (X1) and last value (XN) with the specified lag one autocorrelation
#' in the limit (where N is large) the values are normal with mean 0 and variance 1
#'
genmissing_fast <- function(X1, XN, rho, N, Lfull) {
  
  # Degenerate case: no interior points to simulate, just return the endpoints
  if (N == 2) return(c(X1, XN))
  
  s2 <- 1 - rho^2
  m  <- N - 2 # number of interior points for THIS call
  
  # Slice the precomputed sequence down to what this N needs.
  # Valid because Ldiag[i]/Lsub[i] never depended on N to begin with.
  Ldiag <- Lfull$Ldiag[1:m]
  Lsub  <- Lfull$Lsub[1:m]
  
  # --- Step 1: build the RHS vector b for the conditional-mean equation ---
  # The interior precision matrix only connects to the endpoints through
  # its two boundary entries: interior point 2 connects to X1, and
  # interior point N-1 connects to XN. Everywhere else, conditioning on
  # the endpoints contributes nothing (that's the Markov property at work).
  b <- numeric(m)
  if (m == 1) {
    # special case: with only one interior point, it's adjacent to BOTH endpoints
    b[1] <- (rho / s2) * (X1 + XN)
  } else {
    b[1] <- (rho / s2) * X1
    b[m] <- (rho / s2) * XN
  }
  
  # --- Step 2: solve Omega_II %*% mu = b for the conditional mean mu ---
  # This is done via the standard two-pass bidiagonal solve using L:
  # forward pass solves L %*% w = b (from top down)...
  w <- numeric(m)
  w[1] <- b[1] / Ldiag[1]
  if (m > 1) for (i in 2:m) w[i] <- (b[i] - Lsub[i] * w[i - 1]) / Ldiag[i]
  
  # ...then backward pass solves L^T %*% mu = w (from bottom up)
  mu <- numeric(m)
  mu[m] <- w[m] / Ldiag[m]
  if (m > 1) for (i in (m - 1):1) mu[i] <- (w[i] - Lsub[i + 1] * mu[i + 1]) / Ldiag[i]
  
  # --- Step 3: sample the conditional noise term ---
  # We want interior draws distributed as N(mu, Omega_II^{-1}).
  # If z ~ iid N(0,1) and we solve L^T %*% y = z, then
  # Cov(y) = (L L^T)^{-1} = Omega_II^{-1}, exactly the covariance we need.
  # This is the same backward-substitution pattern as the mu step above,
  # just with random z instead of the deterministic w.
  z <- stats::rnorm(m)
  y <- numeric(m)
  y[m] <- z[m] / Ldiag[m]
  if (m > 1) for (i in (m - 1):1) y[i] <- (z[i] - Lsub[i + 1] * y[i + 1]) / Ldiag[i]
  
  # --- Step 4: combine mean + noise, and re-attach the fixed endpoints ---
  c(X1, mu + y, XN)
}

#' precompute_L
#'
#' Builds the bidiagonal Cholesky factor of the tridiagonal PRECISION matrix 
#' shared by every call of genmissing_fast for a given value of rho.
#'
#'
#' @param rho the lag one autocorrelation
#' @param N_max the maximum number of days between samples
#' @export
#' @return named list with elements Ldiag a numeric vector of the diagonal of the Cholesky factor and Lsub a numeric vector of the subdiagonal of the Cholesky factor
#'
precompute_L <- function(rho, N_max) {
  s2 <- 1 - rho^2            # AR(1) innovation variance, sigma^2 = 1 - rho^2
  m  <- N_max - 2            # number of interior points for the largest N
  d  <- (1 + rho^2) / s2     # diagonal entry of the interior precision matrix
  o  <- -rho / s2            # off-diagonal entry of the interior precision matrix
  
  Ldiag <- numeric(m)        # will hold L[i,i], the diagonal of the Cholesky factor
  Lsub  <- numeric(m)        # will hold L[i,i-1], the subdiagonal of the Cholesky factor
  
  # First diagonal entry has no subdiagonal neighbor to account for
  Ldiag[1] <- sqrt(d)
  
  # Standard bidiagonal Cholesky recursion for a tridiagonal matrix:
  # off-diagonal of Omega = L[i,i-1] * L[i-1,i-1]  ->  solve for L[i,i-1]
  # diagonal of Omega     = L[i,i]^2 + L[i,i-1]^2  ->  solve for L[i,i]
  if (m > 1) {
    for (i in 2:m) {
      Lsub[i]  <- o / Ldiag[i - 1]
      Ldiag[i] <- sqrt(d - Lsub[i]^2)
    }
  }
  
  list(Ldiag = Ldiag, Lsub = Lsub)
}

#' plotWRTDSKalman
#'
#' Two plots to check the flux estimates using the WRTDS_K vs classic WRTDS.
#' The first is annual flux over time, where the two fluxes are shown in different colors.
#' The second is WRTDS vs WRTDSKalman flux estimates. The graphs can be output
#' either on top of each other, or side by side using the \code{sideBySide} argument.
#'
#'
#' @param eList named list with at least the Daily, Sample, and INFO dataframes. This
#' eList must be run through \code{WRTDSKalman}.
#' @param sideBySide logical. If \code{TRUE}, the two plots will be plotted
#' side by side, otherwise, one by one vertically.
#' @param fluxUnit number representing entry in pre-defined fluxUnit class array. \code{\link{printFluxUnitCheatSheet}}
#' @param usgsStyle logical option to use USGS style guidelines. Setting this option
#' to TRUE does NOT guarantee USGS compliance. It will only change automatically
#' generated labels
#'
#' @export
#' @examples
#'
#' eList <- Choptank_eList
#' eList <- WRTDSKalman(eList, niter = 10)
#' plotWRTDSKalman(eList)
#'
#' plotWRTDSKalman(eList, sideBySide = TRUE)
#'
plotWRTDSKalman <- function(
    eList,
    sideBySide = FALSE,
    fluxUnit = 9,
    usgsStyle = FALSE
) {
  if (!all((c("GenFlux", "GenConc") %in% names(eList$Daily)))) {
    stop("This function requires running WRTDSKalman on eList")
  }
  
  if (all(c("paStart", "paLong") %in% names(eList$INFO))) {
    paLong <- eList$INFO$paLong
    paStart <- eList$INFO$paStart
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  AnnualResults <- setupYears(eList$Daily)
  
  if (is.numeric(fluxUnit)) {
    fluxUnit <- fluxConst[shortCode = fluxUnit][[1]]
  } else if (is.character(fluxUnit)) {
    fluxUnit <- fluxConst[fluxUnit][[1]]
  }
  
  if (usgsStyle) {
    yLab <- fluxUnit@unitName[[1]]
  } else {
    yLab <- fluxUnit@shortName[[1]]
  }
  yLab <- trimws(yLab, which = "left")
  unitFactorReturn <- fluxUnit@unitFactor
  
  AnnualResults$Flux <- AnnualResults$Flux * unitFactorReturn
  AnnualResults$GenFlux <- AnnualResults$GenFlux * unitFactorReturn
  
  yMax <- 1.1 * max(AnnualResults$Flux, AnnualResults$GenFlux)
  nYears <- length(AnnualResults[, 1])
  # first a plot of just the WRTDS estimate
  xMin <- floor(AnnualResults$DecYear[1])
  xMax <- ceiling(AnnualResults$DecYear[nYears])
  xlim <- c(xMin, xMax)
  if (sideBySide) {
    title1 <- "Annual Flux Estimates: WRTDS in red, WRTDS-K in green"
    title2 <- "Comparison of the two flux estimates"
    mainTitle <- paste(
      eList$INFO$shortName,
      eList$INFO$paramShortName,
      "\n",
      setSeasonLabelByUser(paStartInput = paStart, paLongInput = paLong)
    )
    par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
    xlab <- paste0("WRTDS annual flux, in ", yLab)
    ylab <- paste0("WRTDSKalman annual flux, in ", yLab)
  } else {
    title1 <- paste(
      eList$INFO$shortName,
      eList$INFO$paramShortName,
      "\nAnnual Flux Estimates: WRTDS in red, WRTDS-K in green\n",
      setSeasonLabelByUser(paStartInput = paStart, paLongInput = paLong),
      sep = "  "
    )
    title2 <- paste(
      eList$INFO$shortName,
      eList$INFO$paramShortName,
      "\nComparison of the two flux estimates\n",
      setSeasonLabelByUser(paStartInput = paStart, paLongInput = paLong),
      sep = "  "
    )
    xlab <- paste0("WRTDS estimate of annual flux, in ", yLab)
    ylab <- paste0("WRTDSKalman estimate of annual flux, in ", yLab)
  }
  
  genericEGRETDotPlot(
    AnnualResults$DecYear,
    AnnualResults$Flux,
    plotTitle = title1,
    tinyPlot = sideBySide,
    xlim = xlim,
    xaxs = "i",
    ylim = c(0, yMax),
    cex.main = 0.9,
    xlab = "",
    ylab = paste0("Annual flux, in ", yLab),
    col = "red",
    cex = 1.4
  )
  points(
    AnnualResults$DecYear,
    AnnualResults$GenFlux,
    col = "green",
    pch = 20,
    cex = 1.4
  )
  
  # scatter plot
  genericEGRETDotPlot(
    AnnualResults$Flux,
    AnnualResults$GenFlux,
    cex = 1.3,
    col = "red",
    xlim = c(0, yMax),
    ylim = c(0, yMax),
    tinyPlot = sideBySide,
    xlab = xlab,
    ylab = ylab,
    cex.main = 0.9,
    plotTitle = title2
  )
  graphics::abline(a = 0, b = 1)
  
  if (sideBySide) {
    mtext(mainTitle, line = -1, side = 3, outer = TRUE, cex = 1)
    par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
  }
}


#' plotTimeSlice
#'
#' Plot of either concentration or flux over time showing both the WRTDS and WRTDSKalman estimates.
#'
#'
#'
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes. This
#' eList must be run through \code{WRTDSKalman}.
#' @param conc logical. If \code{TRUE}, plot concentration, otherwise plot flux.
#' @param start numeric start of DecYear for plot. If \code{NA}, plot will start at the earliest date in the record.
#' @param end numeric end of DecYear for plot. If \code{NA}, plot will end at the latest date in the record.
#' @param usgsStyle logical option to use USGS style guidelines. Setting this option
#' to TRUE does NOT guarantee USGS compliance. It will only change automatically
#' generated labels
#' @param fluxUnit number representing entry in pre-defined fluxUnit class array. \code{\link{printFluxUnitCheatSheet}}
#' @details
#' In the plot title, Ratio of means is mean of WRTDSKalman estimates to the WRTDS Classic estimates.  Ratio only calculated on the data shown in the figure, not the whole series.
#' In the plot, red dots are measured values, blue dots are plotted at the reporting limit for those values that are censored.
#' @examples
#' eList <- Choptank_eList
#' eList <- WRTDSKalman(eList, niter = 10)
#'
#' plotTimeSlice(eList, start = 1990, end = 1991, conc = TRUE)
#'
#' plotTimeSlice(eList, start = 1990, end = 1991, conc = FALSE)
#'
#' plotTimeSlice(eList, start = NA, end = 1991, conc = FALSE)
#'
plotTimeSlice <- function(
    eList,
    start = NA,
    end = NA,
    conc = TRUE,
    fluxUnit = 3,
    usgsStyle = FALSE
) {
  if (!all((c("GenFlux", "GenConc") %in% names(eList$Daily)))) {
    stop("This function requires running WRTDSKalman on eList")
  }
  
  eList <- makeAugmentedSample(eList)
  
  Daily <- eList$Daily
  Sample <- eList$Sample
  
  if (!is.na(start)) {
    Daily <- Daily[Daily$DecYear >= start, ]
    
    Sample <- Sample[Sample$DecYear >= start, ]
  } else {
    start <- min(c(Daily$DecYear, Sample$DecYear))
  }
  
  if (!is.na(end)) {
    Daily <- Daily[Daily$DecYear <= end, ]
    
    Sample <- Sample[Sample$DecYear <= end, ]
  } else {
    end <- max(c(Daily$DecYear, Sample$DecYear))
  }
  
  # figure out which data symbol to use, red for uncensored, brown for censored
  Sample$color <- ifelse(Sample$Uncen == 1, "red", "cyan4")
  
  # first concentration, then flux
  name <- paste(eList$INFO$shortName, eList$INFO$paramShortName)
  
  possibleGoodUnits <- c(
    "mg/l",
    "mg/l as N",
    "mg/l as NO2",
    "mg/L",
    "mg/l as NO3",
    "mg/l as P",
    "mg/l as PO3",
    "mg/l as PO4",
    "mg/l as CaCO3",
    "mg/l as Na",
    "mg/l as H",
    "mg/l as S",
    "mg/l NH4"
  )
  
  allCaps <- toupper(possibleGoodUnits)
  localUnits <- toupper(eList$INFO$param.units)
  
  if (!(localUnits %in% allCaps)) {
    warning(
      "Expected concentration units are mg/l, \nThe INFO dataframe indicates:",
      eList$INFO$param.units,
      "\nFlux calculations will be wrong if units are not consistent"
    )
  }
  
  if (conc) {
    ratio <- mean(Daily$GenConc) / mean(Daily$ConcDay)
    fratio <- format(ratio, digits = 2)
    
    y1 <- Daily$ConcDay
    y2 <- Daily$GenConc
    y3 <- Sample$rObserved
    
    plotTitle <- paste(
      name,
      "\nConcentrations, Black is WRTDS, Green is WRTDSKalman\nData in red, (rl in blue if <), Ratio of means is",
      fratio
    )
  } else {
    if (is.numeric(fluxUnit)) {
      fluxUnit <- fluxConst[shortCode = fluxUnit][[1]]
    } else if (is.character(fluxUnit)) {
      fluxUnit <- fluxConst[fluxUnit][[1]]
    }
    
    fluxFactor <- fluxUnit@unitFactor
    
    ratio <- mean(Daily$GenFlux) / mean(Daily$FluxDay)
    fratio <- format(ratio, digits = 2)
    
    y1 <- Daily$FluxDay * fluxFactor
    y2 <- Daily$GenFlux * fluxFactor
    y3 <- Sample$rObserved * Sample$Q * fluxFactor * 86.40
    
    yLab <- ifelse(usgsStyle, fluxUnit@unitUSGS, fluxUnit@unitExpress)
    
    plotTitle <- paste(
      name,
      "\nFlux, Black is WRTDS, Green is WRTDSKalman\nData in red, (rl in blue if <), Ratio of means is",
      fratio
    )
  }
  
  high_y <- max(y1, y2, y3, na.rm = TRUE)
  low_y <- min(y1, y2, y3, na.rm = TRUE)
  
  yInfo <- generalAxis(
    x = y1,
    minVal = low_y,
    maxVal = high_y,
    padPercent = 10,
    units = eList$INFO$param.units,
    logScale = TRUE,
    concentration = conc
  )
  
  genericEGRETDotPlot(
    Daily$DecYear,
    y1,
    log = "y",
    type = "l",
    xlim = c(start, end),
    ylim = c(yInfo$bottom, yInfo$top),
    yTicks = yInfo$ticks,
    xlab = "",
    cex.main = 0.9,
    ylab = ifelse(conc, yInfo$label, yLab),
    plotTitle = plotTitle
  )
  lines(Daily$DecYear, y2, col = "green")
  points(Sample$DecYear, y3, pch = 20, cex = 1.1, col = Sample$color)
}
