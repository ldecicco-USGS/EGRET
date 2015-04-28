#' Graph of the standard deviation of the log of daily discharge versus year
#'
#' @description
#' Graph of the standard deviation of the log of daily discharge versus year
#' 
#'  Although there are a lot of optional arguments to this function, most are set to a logical default.
#'  
#' Data come from named list, which contains a Daily dataframe with the daily flow data,
#' and an INFO dataframe with metadata. 
#'
#' @param eList named list with at least the Daily and INFO dataframes
#' @param yearStart numeric is the calendar year of the first value to be included in graph, default is NA, which plots from the start of the period of record
#' @param yearEnd numeric is the calendar year of the last value to be included in graph, default is NA, which plots to the end of the period of record
#' @param window numeric which is the full width, in years, of the time window over which the standard deviation is computed, default = 15
#' @param sdMax numeric is the maximum value to be used on the vertical axis of the graph, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure), default is TRUE
#' @param tinyPlot logical variable if TRUE plot is designed to be small, if FALSE it is designed for page size, default is FALSE (not fully implemented yet)
#' @param printStaName logical variable, if TRUE print the station name, if FALSE do not, default is TRUE
#' @param printPA logical variable, if TRUE print the period of analysis information in the plot title, if FALSE leave it out, default is TRUE
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param lwd line width, a positive number, defaulting to 1
#' @param USGSstyle logical use USGSwsGraph package for USGS style
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics streamflow statistics
#' @export
#' @seealso \code{\link{selectDays}}, \code{\link{genericEGRETDotPlot}}
#' @examples
#' eList <- Choptank_eList
#' \dontrun{
#' # Water year:
#' plotSDLogQ(eList) 
#' plotSDLogQ(eList, 1998,2000) 
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotSDLogQ(eList) 
#' library(smwrGraphs)
#' setPDF(basename="plotSDLogQ")
#' layoutInfo <- setLayout(width=4, height=4)
#' layoutStuff <- setGraph(1, layoutInfo)
#' plotSDLogQ(eList, USGSstyle=TRUE) 
#' graphics.off()
#' 
#' }
plotSDLogQ<-function(eList, yearStart=NA,yearEnd=NA,window=15,sdMax=NA,
                     printTitle = TRUE, tinyPlot = FALSE, 
                     printStaName = TRUE, printPA = TRUE, cex=0.8,
                     cex.main=1.1,cex.axis = 1.1,lwd=2, 
                     customPar=FALSE, USGSstyle=FALSE,...){

  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  
  numDays<-length(localDaily$LogQ)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  localDaily <- if(paLong == 12) localDaily else selectDays(localDaily,paLong,paStart)
  numDays<-length(localDaily$LogQ)
  startDec<-localDaily$DecYear[1]
  endDec<-localDaily$DecYear[numDays]
  startDays<-seq(startDec,endDec-window,0.1)
  numResults<-length(startDays)
  y<-rep(NA,numResults)
  xmid<-startDays+(window/2)
  xDates <- rep(NA, numResults)
  
  for (i in 1:numResults){
    firstDay<-startDays[i]
    lastDay<-startDays[i]+window
    smallDaily<-localDaily[localDaily$DecYear >= firstDay & localDaily$DecYear <= lastDay,]
    y[i]<-sd(smallDaily$LogQ,na.rm=TRUE)
    xDates[i] <- mean(as.Date(smallDaily$Date))
  }


  xMin<-if(is.na(yearStart)) startDec else yearStart
  xMax<-if(is.na(yearEnd)) endDec else yearEnd

  line1<-if(printStaName) localINFO$shortName else ""
  line2<- paste(setSeasonLabelByUser(paStartInput = localINFO$paStart, paLongInput = localINFO$paLong))
  line3<-"\nDischarge variability: Standard Deviation of Log(Q)" 
  title<-if(printTitle) paste(line1,if(printPA) paste0("\n",line2) else "",line3) else ""
    
  if(tinyPlot){
    title<-if(printTitle) "standard deviation of log(Q)"
  }

  ##############################################
  
  xInfo <- generalAxis(x=xmid, minVal=yearStart, maxVal=yearEnd, tinyPlot=tinyPlot,padPercent=0)
  yInfo <- generalAxis(x=y, minVal=0, maxVal=sdMax, tinyPlot=tinyPlot,padPercent=5)

  if(USGSstyle){
    names(localINFO) <- gsub("\\.","_",names(localINFO))
    names(localINFO) <- tolower(names(localINFO))
    
    titleUSGS <- paste0(line1,"(",localINFO$site_no,")",if(printPA)paste(" : ",line2),line3)
    
    currentPlot <- timePlot(as.Date(xDates, origin="1970-01-01"), y, Plot=list(what="line"),
                            yaxis.range=c(yInfo$bottom,yInfo$top), ytitle="Dimensionless",xtitle="",
                            xaxis.range=c(as.Date(paste0(floor(xInfo$bottom),"-01-01")),as.Date(paste0(floor(xInfo$top),"-12-31"))),
                            ...)
    
    xMid <- mean(currentPlot$xax$range)    
    yTop <- 0.9*diff(currentPlot$yax$range)+min(currentPlot$yax$range)
    
    if (!tinyPlot) addCaption(titleUSGS)
    invisible(titleUSGS)
  } else {
    genericEGRETDotPlot(x=xmid,#localDaily$DecYear,
                        y=y,
                        xlim=c(xInfo$bottom,xInfo$top),ylim=c(yInfo$bottom,yInfo$top),
                        xlab="",ylab="Dimensionless",xDate=TRUE,
                        xTicks=xInfo$ticks,yTicks=yInfo$ticks,cex=cex,tinyPlot=tinyPlot,
                        plotTitle=title, cex.main=cex.main, cex.axis = cex.axis,
                        type="l", lwd=lwd, customPar=customPar, ...
    )
  }

}