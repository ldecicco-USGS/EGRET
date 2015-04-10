#' plots 1 of the 15 graphs of streamflow statistics on a single page
#'
#' Part of the flowHistory system.  The 15 graphs include annual and four seasonal graphs
#' for each of 3 flow statistics: 1-day maximum, mean, and 7-day minimum
#' @param yearStart A numeric value for the year in which the graph should start
#' @param yearEnd A numeric value for the year in which the graph should end
#' @param qf a scale factor to convert discharge in cubic feet per second to mm/day
#' @param istat A numeric value selecting the flow statistic to be plotted, must be an integer from 1 to 8
#' @param eList named list with at least the Daily and INFO dataframes
#' @param isBottom logical, if TRUE the graph is from the bottom row and thus needs x axis labels, if FALSE it does not need labels
#' @param legend logical, if TRUE add legend.
#' @param USGSstyle logical use USGSwsGraph package for USGS style
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics streamflow
#' @export
#' @examples
#' eList <- Choptank_eList
#' plot1of15(eList, 1990, 2000, 0.2938476,5)
#' \dontrun{
#' library(smwrGraphs)
#' setPDF(basename="plot1")
#' plot1of15(eList, 1990, 2000, 0.2938476,5, USGSstyle=TRUE, legend=TRUE)
#' graphics.off()
#' }
plot1of15<-function(eList, yearStart,yearEnd,qf,istat,
                    isBottom=FALSE, USGSstyle=FALSE, legend=FALSE, ...) {
  
  localINFO <- getInfo(eList)
  
  if("edgeAdjust" %in% names(localINFO)){
    edgeAdjust <- localINFO$edgeAdjust
  } else {
    edgeAdjust <- TRUE
  }
  
  localAnnualSeries <- makeAnnualSeries(eList, edgeAdjust = edgeAdjust)
  
  xSpan<-c(yearStart,yearEnd)
  xTicks<-pretty(xSpan,n=6)
  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  x<-localAnnualSeries[1,istat,]
  y<-qf*localAnnualSeries[2,istat,]
  yTop<-1.05*max(y,na.rm=TRUE)
  yTicks<-yPretty(yTop)
  numYTicks<-length(yTicks)
  yTop<-yTicks[numYTicks]
  
  if(USGSstyle){
    x_year <- floor(x)
    x_seconds <-  x - x_year
    sec_per_year <- unclass(ISOdate(x_year+1,1,1,0,0,0)) - unclass(ISOdate(x_year,1,1,0,0,0)) 
    
    x <- ISOdate(x_year,1,1,0,0,0) + x_seconds * sec_per_year 
    x <- as.Date(x)
  
    xLeft <- as.Date(paste(yearStart,"01","01",sep="-"))
    xRight <- as.Date(paste(yearEnd,"12","31",sep="-"))
    
    currentPlot <- timePlot(x, y,Plot=list(name="Streamflow statistic", 
                                           what="points",
                                           size=0.03),
                            yaxis.range=c(0,yTop), 
                            xaxis.range=c(xLeft,xRight),
                            ytitle="",
                             ...)
    ySmooth<-qf*localAnnualSeries[3,istat,]
    currentPlot <- addXY(x, ySmooth, Plot=list(name="Locally-weighted smooth", what="line"), 
                         currentPlot )
    if(legend) addExplanation(currentPlot, where="ul",title="", box.off = TRUE )
    return(currentPlot)
    
  } else {
    plot(x,y,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",ylim=c(0,yTop),yaxs="i",ylab="",xlab="",main="",type="p")
    if(isBottom) axis(1,tcl=0.5,at=xTicks,labels=xTicks) else axis(1,tcl=0.5,at=xTicks,labels=FALSE)
    axis(2,tcl=0.5,at=yTicks,labels=TRUE)
    axis(3,tcl=0.5,at=xTicks,labels=FALSE)
    axis(4,tcl=0.5,at=yTicks,labels=FALSE)
    y<-qf*localAnnualSeries[3,istat,]
    par(new=TRUE)
    plot(x,y,axes=FALSE,xlim=c(xLeft,xRight),xaxs="i",ylim=c(0,yTop),yaxs="i",ylab="",xlab="",main="",type="l")
    axis(1,tcl=0.5,at=xTicks,labels=FALSE)
    axis(2,tcl=0.5,at=yTicks,labels=FALSE)
    axis(3,tcl=0.5,at=xTicks,labels=FALSE)
    axis(4,tcl=0.5,at=yTicks,labels=FALSE)
    box()
  }
}
