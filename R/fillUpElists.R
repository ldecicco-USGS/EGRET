#' fillUpElists
#'
#' Function used in `runSeries` to populate the yHat, SE, and ConcHat
#' columns into the Daily and Sample data frames within the eList.
#'
#' @param eList named list with the INFO, Daily, and Sample dataframes and surfaces matrix
#' @export
#' @examples
#'
#' eList <- Choptank_eList
#' eList$Daily$yHat <- NULL
#' eList$Daily$SE <- NULL
#' eList$Sample$SE <- NULL
#' eList$Sample$yHat <- NULL
#'
#' eList <- fillUpElists(eList)
#' head(eList$Daily$yHat)
#' head(eList$Daily$SE)
#' head(eList$Sample$SE)
#' head(eList$Sample$yHat)
#'
fillUpElists <- function(eList) {
  # the purpose of this function is to make sure that the eList that comes out of runSeries
  # contains all the columns in Daily and Sample as you get from modelEstimation
  localSample <- getSample(eList)
  localDaily <- getDaily(eList)
  localsurfaces <- getSurfaces(eList)

  if ("LogQ" %in% names(attributes(localsurfaces))) {
    LogQ <- attr(localsurfaces, "LogQ")
  }

  localINFO <- getInfo(eList)
  LogQ <- seq(
    localINFO$bottomLogQ,
    by = localINFO$stepLogQ,
    length.out = localINFO$nVectorLogQ
  )

  if ("Year" %in% names(attributes(localsurfaces))) {
    Year <- attr(localsurfaces, "Year")
  } else {
    localINFO <- getInfo(eList)
    Year <- seq(
      localINFO$bottomYear,
      by = localINFO$stepYear,
      length.out = localINFO$nVectorYear
    )
  }

  localSample$yHat <- fields::interp.surface(
    obj = list(x = LogQ, y = Year, z = localsurfaces[,, 1]),
    loc = data.frame(localSample$LogQ, localSample$DecYear)
  )

  localSample$SE <- fields::interp.surface(
    obj = list(x = LogQ, y = Year, z = localsurfaces[,, 2]),
    loc = data.frame(localSample$LogQ, localSample$DecYear)
  )

  localSample$ConcHat <- fields::interp.surface(
    obj = list(x = LogQ, y = Year, z = localsurfaces[,, 3]),
    loc = data.frame(localSample$LogQ, localSample$DecYear)
  )

  localDaily$yHat <- fields::interp.surface(
    obj = list(x = LogQ, y = Year, z = localsurfaces[,, 1]),
    loc = data.frame(localDaily$LogQ, localDaily$DecYear)
  )

  localDaily$SE <- fields::interp.surface(
    obj = list(x = LogQ, y = Year, z = localsurfaces[,, 2]),
    loc = data.frame(localDaily$LogQ, localDaily$DecYear)
  )

  eList <- as.egret(
    INFO = localINFO,
    Daily = localDaily,
    Sample = localSample,
    surfaces = localsurfaces
  )

  return(eList)
}
