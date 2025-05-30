#' Runs a comparison of any two years in the record.
#' 
#' \code{runPairs} provides comparisons of results, in terms of 
#' flow-normalized concentration and flow-normalized flux for any pair 
#' of years in the water quality record.  Comparison could involve the 
#' use of the "wall" and/or use of "generalized flow normalization".  
#' These two concepts are described in detail in the vignette:
#' \code{vignette("Enhancements", package = "EGRET")}.
#' 
#' @details
#' When using generalized flow-normalization, it is best to have the Daily data frame
#' extend well beyond the years that are in the Sample data frame.  Ideally, 
#' the Daily data frame would start windowSide years before the
#' start of the Sample data set, if the data exist to provide for that. Generally
#' that isn't possible for the end of the record because the Sample data
#' may end very close to the present. To the extent that is possible therefore, it is better to
#' include more discharge data after the end of the Sample record. 
#' Also note that in the case run in the examples don't do that, 
#' because the data set needs to be appropriate for stationary flow 
#' normalization as well (and package size considerations make it difficult to
#' include specialized examples).
#' 
#' @export
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param year1 integer the ending year of the first year in the pair
#' @param year2 integer the ending year of the second year in the pair
#' @param windowSide integer. The width of the flow normalization window on each side of the year being estimated.
#' A common value is 11, but no default is specified.  If stationary flow normalization is to be used, then windowSide = 0 (this means that 
#' flow-normalization period for all years is the same).
#' @param flowBreak logical. Is there an abrupt break in the discharge record, default is FALSE.
#' @param Q1EndDate The Date (as character in YYYY-MM-DD) which is the last day, just before the flowBreak.
#' @param QStartDate The first Date (as character in YYYY-MM-DD) used in the  flow normalization method.  Default is 
#' NA, which makes the QStartDate become the first Date in eList$Daily. 
#' @param QEndDate The last Date (as character in YYYY-MM-DD) used in the flow normalization method.  Default is NA, 
#' which makes the QEndDate become the last Date in eList$Daily.
#' @param wall logical. Whether there is an abrupt break in the concentration versus discharge relationship due to some major change in 
#' pollution control or water management.  Default is FALSE.
#' @param sample1EndDate The Date (as character in YYYY-MM-DD) of the last date just before the wall. Default = NA. 
#' A date must be specified if wall = TRUE.
#' @param sampleStartDate The Date (as character in YYYY-MM-DD) of the first sample to be used. Default is NA which sets it 
#' to the first Date in eList$Sample.
#' @param sampleEndDate The Date (as character in YYYY-MM-DD) of the last sample to be used. 
#' Default is NA which sets it to the last Date in eList$Sample.
#' @param paLong numeric integer specifying the length of the period of analysis, in months, 1<=paLong<=12. 
#' Default is NA, which will use the paLong in the eList$INFO data frame. See also \code{\link{setPA}}.
#' @param paStart numeric integer specifying the starting month for the period of analysis, 1<=paStart<=12.
#' Default is NA, which will use the paStart in the eList$INFO data frame. See also \code{\link{setPA}}.
#' @param windowY numeric specifying the half-window width in the time dimension, in units of years, default is 7
#' @param windowQ numeric specifying the half-window width in the discharge dimension, units are natural log units, default is 2
#' @param windowS numeric specifying the half-window with in the seasonal dimension, in units of years, default is 0.5
#' @param minNumObs numeric specifying the miniumum number of observations required to run the weighted regression, default is 100
#' @param minNumUncen numeric specifying the minimum number of uncensored observations to run the weighted regression, default is 50
#' @param fractMin numeric specifying the minimum fraction of the observations required to run the weighted regression, default is 0.75. The
#' minimum number will be the maximum of minNumObs and fractMin multiplied by total number of observations.
#' @param edgeAdjust logical specifying whether to use the modified method for calculating the windows at the edge of the record.  
#' The edgeAdjust method tends to reduce curvature near the start and end of record.  Default is TRUE.
#' @param oldSurface logical specifying whether to use the original surface, or create a new one. Default is FALSE.
#' @param verbose logical specifying whether or not to display progress message
#' @param saveOutput logical. If \code{TRUE}, a text file will be saved in the working directory of the printout of
#' what is in the console output. Default is \code{FALSE}.
#' @param fileName character. Name to save the output file if \code{saveOutput=TRUE}.
#' @return Data frame with 7 columns and 2 rows.  The first row is about trends in concentration (mg/L), 
#' the second column is about trends in flux (million kg/year).  
#' The data frame has a number of attributes.
#' \tabular{ll}{
#' Column Name \tab Description \cr
#' Total Change \tab   The difference between the results for year2 - year1 (x22 - x11)\cr
#' CQTC \tab CQTC is the "Concentration v. Q Trend Component." It is the component of total 
#' change due to the change in the CQR (Concentration Discharge Relationship). (x20 - x10). \cr
#' QTC \tab  QTC is the "Q Trend Component." It is the component of total change due to the
#' trend in the QD (Discharge Distribution). (x22 - x11 - x20 + x10).  \cr
#' x10 \tab The estimated value based on the CQR computed for year1, integrated over the QD for the entire 
#' timespan of the Daily data frame (or the period QStartDate and to QEndDate if these are specified).\cr
#' x11 \tab The estimated value based on the CQR for year1, integrated over the QD specified by the user for year1.\cr
#' x20 \tab The estimated value based on the CQR computed for year2, integrated over the QD for the entire period of record. \cr
#' x22 \tab The estimated value based on the CQR for year2, integrated over the QD specified by the user for year2. \cr
#' }
#' Additionally, there is an attribute on the data frame "Other", containing
#' a list that includes minNumObs=minNumObs, minNumUncen, windowY, windowQ, siteName,
#' windowS, wall, edgeAdjust, QStartDate, QEndDate, PercentChangeConc, and PercentChangeFlux.
#' 
#' PercentChangeConc, and PercentChangeFlux are vectors where:
#' Total Percent Change is the Total Change divided by x11
#' CQTC Percent is the CQTC divided by x11
#' QTC Percent  is the QTC divided by x11
#' 
#' Another attribute is "byMonth".  This is a data frame of 4 columns and 14 rows.  
#' The columns represent the concentrations and fluxes for the starting and ending year.
#' The flux values for each month are flow normalized monthly watershed yields
#' expressed as kg/month/km^2.  The concentrations are the mean flow normalized
#' concentration, expressed in whatever concentration units the raw data are 
#' expressed as (typically mg/L).  This data frame is used as the input to the
#' \code{plotMonthTrend} function. 
#' 
#' @examples 
#' eList <- Choptank_eList
#' year1 <- 1985
#' year2 <- 2010
#' 
#' \donttest{
#' # Automatic calculations based on windowSide = 11
#' # four possible ways to do generalized flow normalization:
#' 
#' #Option 1: Use all years for flow normalization.
#' 
#' pairOut_1 <- runPairs(eList, year1, year2, windowSide = 0)
#' 
#' # Option 2:  Use different windows for flow normalization for year1 versus year2
#' #            In each case it is a 23 year window (23 = 1 + 2*11)
#' 
#' pairOut_2 <- runPairs(eList, year1, year2, windowSide = 11)
#' 
#' # Option 3: Flow normalization is based on splitting the flow record at 1990-09-30
#' #          But year1 uses all flow data from before the break, 
#' #          year2 uses all flow data after the break
#' 
#' pairOut_3 <- runPairs(eList, year1, year2, 
#'                       windowSide = 0, flowBreak = TRUE,
#'                       Q1EndDate = "1990-09-30")
#' 
#' # Option 4: Flow normalization is based on splitting the flow record at 1990-09-30
#' #           but year1 uses a 23 year window before the break
#' #           year2 uses a 23 year window after the break
#' 
#' pairOut_4 <- runPairs(eList, year1, year2, 
#'                       windowSide = 11, flowBreak = TRUE,
#'                       Q1EndDate = "1990-09-30")
#'                       
#' monthly_trends <- attr(pairOut_4, "byMonth")  
#' plotMonthTrend(pairOut_4)
#' 
#' eList <- setPA(eList, paLong = 3, paStart = 12)
#' pairOut_5 <- runPairs(eList, year1, year2,
#'                       windowSide = 11)
#' monthly_trends <- attr(pairOut_5, "byMonth") 
#' plotMonthTrend(pairOut_5)                      
#' 
#' }
runPairs <- function(eList, year1, year2, windowSide, 
                     flowBreak = FALSE,
                     Q1EndDate = NA, QStartDate = NA, QEndDate = NA, 
                     wall = FALSE, oldSurface = FALSE,
                     sample1EndDate = NA, sampleStartDate = NA, sampleEndDate = NA,
                     paStart = NA, paLong = NA,
                     minNumObs = 100, minNumUncen = 50, fractMin = 0.75,
                     windowY = 7, windowQ = 2, windowS = 0.5, 
                     edgeAdjust = TRUE,
                     saveOutput = FALSE, 
                     fileName = "temp.txt", 
                     verbose = TRUE){
  
  if(wall & oldSurface){
    message("Setting both arguments wall and oldSurfaces to TRUE are not allowed.")
    message("Re-calculating surface.")
    oldSurface <- FALSE
  }
  
  if(!is.egret(eList)){
    stop("Please check eList argument")
  }

  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  
  if(is.na(paStart)){
    if(all(c("paStart") %in% names(eList$INFO))){
      paStart <- eList$INFO$paStart 
    } else {
      paStart <- 10
    }
  } else {
    eList$INFO$paStart <- paStart
  }
  
  if(is.na(paLong)){
    if(all(c("paLong") %in% names(eList$INFO))){
      paLong <- eList$INFO$paLong
    } else {
      paLong <- 12
    }
  } else {
    eList$INFO$paLong <- paLong
  }
  
  startEndSurface1 <- startEnd(paStart, paLong, year1)
  startEndSurface2 <- startEnd(paStart, paLong, year2)
  
  if(startEndSurface2$startDate > range(localSample$Date)[2]){
    stop("year2 is outside the Sample range")
  }
  
  if(startEndSurface1$endDate < range(localSample$Date)[1]){
    stop("year1 is outside the Sample range")
  }
  
  if(is.na(sampleStartDate)){
    sampleStartDate <- localSample$Date[1]
  }  else {
    sampleStartDate <- as.Date(sampleStartDate)
  }
  
  numSamples <- length(localSample$Date)
  
  if(is.na(sampleEndDate)){
    sampleEndDate <- localSample$Date[numSamples]
  }  else {
    sampleEndDate <- as.Date(sampleEndDate)
  }
  
  if(is.na(QStartDate)){
    QStartDate <- localDaily$Date[1]
  } else {
    QStartDate <- as.Date(QStartDate)
  }
  
  numQDays <- length(localDaily$Date)
  
  if(is.na(QEndDate)){
    QEndDate <- localDaily$Date[numQDays]
  } else {
    QEndDate <- as.Date(QEndDate)
  }
  
  if (sampleStartDate > as.Date(startEndSurface1[[2]]) ){
    stop("Sample start is later than year2")
  }  
  
  if (sampleEndDate < as.Date(startEndSurface2[[1]]) ){
    stop("Sample end is earlier than year1")
  }
  localsurfaces <- getSurfaces(eList)
  
  if(oldSurface){
    if(all(is.na(localsurfaces))){
      message("No surface included in eList, running estSurface function")
      oldSurface <- FALSE
    } 
  }
  
  if(flowBreak && is.na(Q1EndDate)) stop("if there is a flowBreak you must provide Q1EndDate")
  
  # setting up the two flow windows
  # there are four cases
  flowNormStartCol <- "flowNormStart"
  flowNormEndCol <- "flowNormEnd"
  flowStartCol <- "flowStart"
  flowEndCol <- "flowEnd"
  
  if (windowSide <= 0 && !flowBreak) {
    flowStart <- c(startEndSurface1[["startDate"]], startEndSurface2[["startDate"]])
    flowEnd <- c(startEndSurface1[["endDate"]], startEndSurface2[["endDate"]])
    flowNormStart <- c(QStartDate, QStartDate)
    flowNormEnd <- c(QEndDate, QEndDate)
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, 
                           flowEnd, stringsAsFactors = FALSE)
  } else if (windowSide > 0 & !flowBreak) {
    dateInfo1 <- makeDateInfo(windowSide, startEndSurface1[["startDate"]], startEndSurface1[["endDate"]], 
                                 QStartDate, QEndDate)
    dateInfo2 <- makeDateInfo(windowSide, startEndSurface2[["startDate"]], startEndSurface2[["endDate"]], 
                                 QStartDate, QEndDate)
    dateInfo <- rbind(dateInfo1, dateInfo2)
  } else if (windowSide <= 0 && flowBreak) {
    Q1EndDate <- as.Date(Q1EndDate)
    Q2StartDate <- Q1EndDate + 1
    flowStart <- c(startEndSurface1[["startDate"]], startEndSurface2[["startDate"]])
    flowEnd <- c(startEndSurface1[["endDate"]], startEndSurface2[["endDate"]])
    flowNormStart <- c(as.Date(QStartDate), as.Date(Q2StartDate))
    flowNormEnd <- c(as.Date(Q1EndDate), as.Date(QEndDate))
    dateInfo <- data.frame(flowNormStart, flowNormEnd, flowStart, 
                           flowEnd, stringsAsFactors = FALSE)
  } else {
    Q1EndDate <- as.Date(Q1EndDate)
    Q2StartDate <- Q1EndDate + 1
    dateInfo1 <- makeDateInfo(windowSide, startEndSurface1[["startDate"]], startEndSurface1[["endDate"]], 
                                 QStartDate, Q1EndDate)
    dateInfo2 <- makeDateInfo(windowSide, startEndSurface2[["startDate"]], startEndSurface2[["endDate"]],
                                 Q2StartDate, QEndDate)
    dateInfo <- rbind(dateInfo1, dateInfo2)
  }
  #
  #   end of flow normalization 
  #
  
  if (wall) {
    if (is.na(sample1EndDate)) {
      stop("When using the wall option, please specify sample1EndDate")
    }
    sample1EndDate <- as.Date(sample1EndDate)
    sample2StartDate <- as.Date(sample1EndDate) + 1
    sample1StartDate <- as.Date(sampleStartDate)
    sample2EndDate <- as.Date(sampleEndDate)
  } else {
    sample1StartDate <- as.Date(sampleStartDate)
    sample2StartDate <- as.Date(sampleStartDate)
    sample1EndDate <- as.Date(sampleEndDate)
    sample2EndDate <- as.Date(sampleEndDate)
  }
  
  Sample1 <- localSample[localSample$Date >= sample1StartDate & 
                           localSample$Date <= sample1EndDate, ]
  Sample2 <- localSample[localSample$Date >= sample2StartDate & 
                           localSample$Date <= sample2EndDate, ]
  
  fractMin <- min(fractMin, 1.0)
  
  minNumObs <- ceiling(min(minNumObs, fractMin * length(Sample1$Date), 
                           fractMin * length(Sample2$Date)))
  minNumUncen <- ceiling(min(0.5 * minNumObs, minNumUncen))
  
  message("Sample1 has ", length(Sample1$Date), " Samples and ", 
          sum(Sample1$Uncen), " are uncensored")
  message("Sample2 has ", length(Sample2$Date), " Samples and ", 
          sum(Sample2$Uncen), " are uncensored")
  message("minNumObs has been set to ", minNumObs, " minNumUncen has been set to ", 
          minNumUncen)
  check <- rep(1,4)
  if(minNumObs > length(Sample1$Date)) check[1] <- 0
  if(minNumObs > length(Sample2$Date)) check[2] <- 0
  if(minNumUncen > sum(Sample1$Uncen)) check[3] <- 0
  if(minNumUncen > sum(Sample2$Uncen)) check[4] <- 0
  
  if(sum(check) < 4) {
    stop("Data set too small for minNumObs or minNumUncen")
  }
  
  message("Sample1 has ", length(Sample1$Date), " Samples and ", 
          sum(Sample1$Uncen), " are uncensored")
  message("Sample2 has ", length(Sample2$Date), " Samples and ", 
          sum(Sample2$Uncen), " are uncensored")
  message("minNumObs has been set to ", minNumObs, " minNumUncen has been set to ", 
          minNumUncen)
  
  Daily1 <- localDaily[localDaily$Date >= dateInfo$flowNormStart[1] & localDaily$Date <= 
                         dateInfo$flowNormEnd[1], ]
  Daily2 <- localDaily[localDaily$Date >= dateInfo$flowNormStart[2] & localDaily$Date <= 
                         dateInfo$flowNormEnd[2], ]
  if(oldSurface){
    
    checkSurfaceSpan(eList)
    
    if(all(c("Year","LogQ","surfaceIndex") %in% names(attributes(localsurfaces)))){
      surfaceYear <- attr(localsurfaces, "Year")
      LogQ <- attr(localsurfaces, "LogQ")
    } else {
      localINFO <- getInfo(eList)
      LogQ <- seq(localINFO$bottomLogQ, by=localINFO$stepLogQ, length.out=localINFO$nVectorLogQ)
      surfaceYear <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
    }
    
    startDec1 <- decimalDate(startEndSurface1[["startDate"]])
    endDec1 <- decimalDate(startEndSurface1[["endDate"]])
    startDec2 <- decimalDate(startEndSurface2[["startDate"]])
    endDec2 <- decimalDate(startEndSurface2[["endDate"]])
    
    surfIndex1 <- which(surfaceYear >= startDec1 & surfaceYear <= endDec1)
    surfIndex1 <- c(surfIndex1[1]-1,surfIndex1,surfIndex1[length(surfIndex1)]+1)
    surfIndex2 <- which(surfaceYear >= startDec2 & surfaceYear <= endDec2)
    surfIndex2 <- c(surfIndex2[1]-1,surfIndex2,surfIndex2[length(surfIndex2)]+1)
    
    surfaces1 <- localsurfaces[,surfIndex1,]
    surfaces2 <- localsurfaces[,surfIndex2,]
    
    attr(surfaces1, "LogQ") <- LogQ
    attr(surfaces1, "Year") <- surfaceYear[surfIndex1]
    
    attr(surfaces2, "LogQ") <- LogQ
    attr(surfaces2, "Year") <- surfaceYear[surfIndex2]
    
  } else {
    surfaces1 <- estSurfaces(eList, 
                             surfaceStart = startEndSurface1[["startDate"]],
                             surfaceEnd = startEndSurface1[["endDate"]], 
                             localSample = Sample1,
                             minNumObs = minNumObs, minNumUncen = minNumUncen, 
                             windowY = windowY, windowQ = windowQ, windowS = windowS, 
                             edgeAdjust = edgeAdjust, verbose = FALSE)
    surfaces2 <- estSurfaces(eList, 
                             surfaceStart = startEndSurface2[["startDate"]],
                             surfaceEnd = startEndSurface2[["endDate"]], 
                             localSample = Sample2,
                             minNumObs = minNumObs, minNumUncen = minNumUncen, 
                             windowY = windowY, windowQ = windowQ, windowS = windowS,
                             edgeAdjust = edgeAdjust, verbose = FALSE)    
  }

  DailyRS1FD1 <- estDailyFromSurfaces(eList, localsurfaces = surfaces1, 
                                      localDaily = Daily1)
  annualFlex <- setupYears(DailyRS1FD1, paLong = paLong, paStart = paStart)
  c11 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f11 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  if(is.nan(c11)){
    stop("year1 does not have adaquate Daily data (missing >10%).
         Adjusting year1.")
  }
  DailyRS2FD2 <- estDailyFromSurfaces(eList, localsurfaces = surfaces2, 
                                      localDaily = Daily2)
  annualFlex <- setupYears(DailyRS2FD2, paLong = paLong, paStart = paStart)
  c22 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f22 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  if(is.nan(c22)){
    stop("year2 does not have adaquate Daily data (missing >10%).
         Adjusting to year2.")
  }
  Daily0 <- localDaily[localDaily$Date >= QStartDate & localDaily$Date <= 
                         QEndDate, ]
  DailyRS1FD0 <- estDailyFromSurfaces(eList, localsurfaces = surfaces1, 
                                      localDaily = Daily0)
  annualFlex <- setupYears(DailyRS1FD0, paLong = paLong, paStart = paStart)
  c10 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f10 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  DailyRS2FD0 <- estDailyFromSurfaces(eList, localsurfaces = surfaces2, 
                                      localDaily = Daily0)
  annualFlex <- setupYears(DailyRS2FD0, paLong = paLong, paStart = paStart)
  c20 <- mean(annualFlex$FNConc, na.rm = TRUE)
  f20 <- mean(annualFlex$FNFlux, na.rm = TRUE)
  cDeltaTotal <- c22 - c11
  cRSpart <- c20 - c10
  cFDpart <- cDeltaTotal - cRSpart
  
  fDeltaTotal <- f22 - f11
  fRSpart <- f20 - f10
  fFDpart <- fDeltaTotal - fRSpart
  
  totChangePct_conc <- 100*cDeltaTotal/c11
  totChangePct_flux <- 100*fDeltaTotal/f11
  
  CQTC_percent_conc <- 100*cRSpart / c11 # CQTC Percent
  QTC_percent_conc  <- 100*cFDpart / c11 # Q Trend Component Percent

  CQTC_percent_flux <- 100*fRSpart / f11
  QTC_percent_flux  <- 100*fFDpart / f11
  
  pairResults <- as.data.frame(matrix(ncol = 7, nrow = 2))
  colnames(pairResults) <- c("TotalChange", "CQTC", "QTC", 
                             "x10", "x11", "x20", "x22")
  rownames(pairResults) <- c("Conc", "Flux")
  pairResults[1, ] <- c(cDeltaTotal, cRSpart, cFDpart, c10, 
                        c11, c20, c22)
  # 0.00036525 is magic number to convert to million kg/year
  pairResults[2, ] <- 0.00036525 * c(fDeltaTotal, fRSpart, 
                                     fFDpart, f10, f11, f20, f22)
  
  yearPairInfo <- c(paStart, paLong, year1, year2)
  names(yearPairInfo) <- c("paStart","paLong","year1","year2")
  attr(pairResults, "yearPair") <- yearPairInfo
  
  attr(pairResults, "dateInfo") <- dateInfo
  
  SampleBlocks <- c(sample1StartDate, sample1EndDate, sample2StartDate, sample2EndDate)
  names(SampleBlocks) <- c("sample1StartDate", "sample1EndDate", "sample2StartDate", "sample2EndDate")
  attr(pairResults, "SampleBlocks") <- SampleBlocks
  
  Other <- list(minNumObs = minNumObs, 
                minNumUncen = minNumUncen, 
                windowY = windowY, 
                windowQ = windowQ, 
                windowS = windowS, 
                wall = wall,
                edgeAdjust = edgeAdjust,
                QStartDate = as.Date(QStartDate), 
                QEndDate = as.Date(QEndDate),
                siteName = eList$INFO$shortName,
                paramShortName = eList$INFO$paramShortName,
                PercentChangeConc = c("Total Percent Change" = totChangePct_conc, 
                                      "CQTC Percent" = CQTC_percent_conc, 
                                      "QTC Percent" = QTC_percent_conc),
                PercentChangeFlux = c("Total Percent Change" = totChangePct_flux, 
                                      "CQTC Percent" = CQTC_percent_flux, 
                                      "QTC Percent" = QTC_percent_flux))

  attr(pairResults, "Other") <- Other
  
  if(saveOutput){
    sink(fileName)
  }
  
  if(verbose) printPairs(eList, pairResults)
  
  if(saveOutput){
    sink()
  }
  
  z <- data.frame(matrix(ncol = 14, nrow = 4))
  colnames(z) <- c("Year", "Type", "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  z$Year <- c(year1, year1, year2, year2)
  z$Type <- c("Flux", "Conc", "Flux", "Conc")
  
  k <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
  eList1 <- as.egret(eList$INFO, DailyRS1FD1, eList$Sample)
  monthlyResults1 <- calculateMonthlyResults(eList1)
  monthlyResults1 <- stats::na.omit(monthlyResults1)
  eList2 <- as.egret(eList$INFO, DailyRS2FD2, eList$Sample)
  monthlyResults2 <- calculateMonthlyResults(eList2)
  monthlyResults2 <- stats::na.omit(monthlyResults2)
  monthlyResults1$monthFlux <- monthlyResults1$nDays * monthlyResults1$FNFlux / eList$INFO$drainSqKm
  monthlyResults1$monthConc <- monthlyResults1$FNConc
  
  monthlyResults2$monthFlux <- monthlyResults2$nDays * monthlyResults2$FNFlux / eList$INFO$drainSqKm
  monthlyResults2$monthConc <- monthlyResults2$FNConc
  months <- paStart:(paStart + paLong - 1)
  months[months > 12] <- months[months > 12] - 12
  for(i in months){

    flux1 <- monthlyResults1$monthFlux[which(monthlyResults1$Month == i)]
    flux1 <- ifelse(length(flux1) == 0, NA, flux1)
    conc1 <- monthlyResults1$monthConc[which(monthlyResults1$Month == i)]
    conc1 <- ifelse(length(conc1) == 0, NA, conc1)
    z[1,i+2] <- flux1
    z[2,i+2] <- conc1

    flux2 <- monthlyResults2$monthFlux[which(monthlyResults2$Month == i)]
    flux2 <- ifelse(length(flux2) == 0, NA, flux2)
    conc2 <- monthlyResults2$monthConc[which(monthlyResults2$Month == i)]
    conc2 <- ifelse(length(conc2) == 0, NA, conc2)
    z[3,i+2] <- flux2
    z[4,i+2] <- conc2
  }
  
  attr(pairResults, "byMonth") <- z
  
  
  return(pairResults)
  
}

#' Print information about pairs analysis
#' 
#' Prints the information from the \code{runPairs} function.
#' This could be used to save the output to a text file.
#' 
#' @param eList named list with at least the Daily, Sample, and INFO dataframes
#' @param pairResults output of \code{runGroups}.
#' @export
#' @return text to console
#' @examples 
#' eList <- Choptank_eList
#' year1 <- 1985
#' year2 <- 2010
#' 
#' \donttest{
#' pairOut_1 <- runPairs(eList, 
#'                       year1, year2,
#'                       windowSide = 0)
#'                        
#' printPairs(eList, pairOut_1)
#'}
#'
printPairs <- function(eList, pairResults){
  
  Other <- attr(pairResults, "Other")
  SampleBlocks <- attr(pairResults, "SampleBlocks") 
  yearPairInfo <- attr(pairResults, "yearPair")
  
  sample1EndDate <- SampleBlocks["SampleBlocks"]
  
  cat("\n  ", eList$INFO$shortName, "\n  ", eList$INFO$paramShortName)
  periodName <- setSeasonLabelByUser(eList$INFO$paStart, eList$INFO$paLong)
  cat("\n  ", periodName, "\n")
  if (Other$wall) 
    cat("\n Sample data set was partitioned with a wall right after ", 
        as.character(sample1EndDate), "\n")
  cat("\n Change estimates ", yearPairInfo[["year2"]], " minus ", yearPairInfo[["year1"]], "\n")
  totChange <- format(pairResults[1, 1], digits = 3)
  cat("\n For concentration: total change is ", totChange, 
      "mg/L")
  totChangePct_conc_f <- add_plus(Other$PercentChangeConc[["Total Percent Change"]])

  cat("\n expressed as Percent Change is ", totChangePct_conc_f)

  pctRS <- add_plus(Other$PercentChangeConc[["CQTC Percent"]])
  pctFD <- add_plus(Other$PercentChangeConc[["QTC Percent"]])
  
  cat("\n\n Concentration v. Q Trend Component ", pctRS, "\n       Q Trend Component            ", 
      pctFD, " \n\n")
  totChange <- format(pairResults[2, 1], digits = 3)

  totChangePct_flux_f <- add_plus(Other$PercentChangeFlux[["Total Percent Change"]])
  
  cat("\n For flux: total change is ", totChange, "million kg/year")
  cat("\n expressed as Percent Change is ", totChangePct_flux_f)

  pctRS <- add_plus(Other$PercentChangeFlux[["CQTC Percent"]])
  pctFD <- add_plus(Other$PercentChangeFlux[["QTC Percent"]])
  
  cat("\n\n Concentration v. Q Trend Component ", pctRS, "\n       Q Trend Component            ", 
      pctFD, " \n\n")
  print(pairResults[,1:7], digits = 2)
}


add_plus <- function(x){
  
  x <- sprintf("%+.2f %%", x)
  
  if(x == "+0.00 %" | x == "-0.00 %"){
    x <- " 0 %"
  }
  
  return(x)
}
