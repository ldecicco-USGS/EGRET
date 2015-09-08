#' Plot of Observed Concentration versus Estimated Concentration 
#'
#' @description
#' Data come from named list, which contains a Sample dataframe with the sample data, 
#' and an INFO dataframe with metadata. 
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default.
#'
#' @param eList named list with at least the Sample and INFO dataframes
#' @param concMax number specifying the maximum value to be used on the vertical axis, default is NA (which allows it to be set automatically by the data)
#' @param printTitle logical variable if TRUE title is printed, if FALSE not printed (this is best for a multi-plot figure)
#' @param tinyPlot logical variable, if TRUE plot is designed to be plotted small, as a part of a multipart figure, default is FALSE
#' @param logScale logical, default TRUE, TRUE indicates y axis is in log scale, "xy" indicates both x and y in log scale, "x" is only x
#' @param cex numerical value giving the amount by which plotting symbols should be magnified
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param cex.axis magnification to be used for axis annotation relative to the current setting of cex
#' @param customPar logical defaults to FALSE. If TRUE, par() should be set by user before calling this function 
#' (for example, adjusting margins with par(mar=c(5,5,5,5))). If customPar FALSE, EGRET chooses the best margins depending on tinyPlot.
#' @param col color of points on plot, see ?par 'Color Specification'
#' @param lwd number line width
#' @param USGSstyle logical use USGSwsGraph package for USGS style
#' @param legend logical add legend
#' @param rResid logical option to plot randomized residuals.
#' @param \dots arbitrary graphical parameters that will be passed to genericEGRETDotPlot function (see ?par for options)
#' @keywords graphics water-quality statistics
#' @seealso \code{\link{selectDays}}, \code{\link{genericEGRETDotPlot}}
#' @export
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' plotConcPred(eList)
#' plotConcPred(eList, logScale=TRUE)
#' plotConcPred(eList, logScale=TRUE, rResid=TRUE)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' plotConcPred(eList)
#' \dontrun{
#' library(smwrGraphs)
#' setPDF(basename = "test")
#' layoutInfo <- setLayout(width=6, height=4)
#' layoutStuff <- setGraph(1, layoutInfo)
#' plotConcPred(eList, USGSstyle=TRUE, margin=layoutStuff)
#' graphics.off()
#' }
plotConcPred<-function(eList, concMax = NA, logScale=FALSE,
                       printTitle = TRUE,tinyPlot=FALSE,cex=0.8, cex.axis=1.1,
                       cex.main=1.1, customPar=FALSE,col="black",lwd=1,
                       USGSstyle=FALSE,legend=FALSE,rResid=FALSE,...){

  localINFO <- getInfo(eList)
  localSample <- getSample(eList) 
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  } 
  
  localSample <- if(paLong == 12) localSample else selectDays(localSample, paLong,paStart)
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  
  x<-localSample$ConcHat

  Uncen<-localSample$Uncen

  if(tinyPlot & !USGSstyle){
    xLab<-"Est. Conc."
    yLab<-"Obs. Conc."
  } else {
    xLab<-paste("Estimated Concentration in",localINFO$param.units)
    yLab<-paste("Observed Concentration in",localINFO$param.units)
  }
  
  if (logScale){
    minYLow <- NA
    minXLow <- NA
    logVariable <- "xy"
  } else {
    minYLow <- 0
    minXLow <- 0
    logVariable <- ""
  } 
  
  plotTitle<-if(printTitle) paste(localINFO$shortName,"\n",localINFO$paramShortName,"\n","Observed versus Estimated Concentration") else ""

  xInfo <- generalAxis(x=x, minVal=minXLow, maxVal=concMax, tinyPlot=tinyPlot,logScale=logScale)  
  
  if(USGSstyle){
    xLab<-paste("Estimated concentration in",localINFO$param.units)
    yLab<-paste("Observed concentration in",localINFO$param.units)
    
    yLow<-localSample$ConcLow
    yHigh<-localSample$ConcHigh
    
    yInfo <- generalAxis(x=yHigh, minVal=minYLow, maxVal=concMax, tinyPlot=tinyPlot,logScale=logScale)
    
    if(col == "black"){
      col <- list("Uncensored"="black","Censored"="gray80")
    }
    Uncen <- localSample$Uncen
    Uncen <- ifelse(Uncen==1, "Uncensored", "Censored")
    col <- col[unique(Uncen)]
    dotSize <- 0.09
    if(tinyPlot){
      dotSize <- 0.03
    }
    currentPlot <- colorPlot(x, yHigh, color= Uncen, Plot=list(what="points",color=col,size=dotSize),
                             yaxis.range=c(yInfo$bottom,yInfo$top), ytitle=yLab,
                             xaxis.range=c(xInfo$bottom, xInfo$top), xtitle=xLab,
                             xaxis.log=logScale,yaxis.log=logScale,
                             ...)
    refLine(coefficients=c(0,1), current=currentPlot)
    
    yTop <- 0.9*diff(currentPlot$yax$range)+min(currentPlot$yax$range)
    
    if(legend) addExplanation(currentPlot, where="ul",title="")
    
    if(logScale){
      x <- transData(data = x[Uncen == "Censored"], TRUE, FALSE)
      xMid <- 10^mean(currentPlot$xax$range)
    } else {
      xMid <- mean(currentPlot$xax$range)
      x <- x[Uncen == "Censored"]
    }
    
#     addBars(x, 
#             yHigh[Uncen == "Censored"], base=min(currentPlot$yax$range), 
#             current=currentPlot, 
#             Bars=list(width=0.01,fill="white",border="gray80"))
    
    
    if (!tinyPlot) addAnnotation(x=xMid, y=yTop,justification="center", 
                                 annotation=title2, current=currentPlot,size=10)
    invisible(currentPlot)
  } else {

    if(!rResid){
      yLow<-localSample$ConcLow
      yHigh<-localSample$ConcHigh
      
      yInfo <- generalAxis(x=yHigh, minVal=minYLow, maxVal=concMax, tinyPlot=tinyPlot,logScale=logScale)
  
      genericEGRETDotPlot(x=x, y=yHigh,
                          xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                          xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                          xlab=xLab, ylab=yLab,log=logVariable,
                          plotTitle=plotTitle, oneToOneLine=TRUE,
                          cex.axis=cex.axis,cex.main=cex.main,cex=cex,
                          tinyPlot=tinyPlot,customPar=customPar,col=col,lwd=lwd,...
        )
    
      censoredSegments(yBottom=yInfo$bottom, yLow=yLow, yHigh=yHigh, x=x, Uncen=Uncen,col=col,lwd=lwd)
    } else {
      if(!("rObserved" %in% names(localSample))){
        eList <- makeAugmentedSample(eList)
        localSample <- eList$Sample
      }
      yHigh <- localSample$rObserved
      
      yInfo <- generalAxis(x=yHigh, minVal=minYLow, maxVal=concMax, tinyPlot=tinyPlot,logScale=logScale)
      
      genericEGRETDotPlot(x=x[Uncen == 1], y=yHigh[Uncen == 1],
                          xTicks=xInfo$ticks, yTicks=yInfo$ticks,
                          xlim=c(xInfo$bottom,xInfo$top), ylim=c(yInfo$bottom,yInfo$top),
                          xlab=xLab, ylab=yLab,log=logVariable,
                          plotTitle=plotTitle, oneToOneLine=TRUE,
                          cex.axis=cex.axis,cex.main=cex.main,cex=cex,
                          tinyPlot=tinyPlot,customPar=customPar,col=col,lwd=lwd,...
      )
      points(x=x[Uncen == 0], y=yHigh[Uncen == 0], pch=1,cex=cex,col=col)
    }
  }
  if (!tinyPlot) mtext(title2,side=3,line=-1.5)
  invisible(eList)

}
