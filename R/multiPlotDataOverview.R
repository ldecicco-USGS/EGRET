#' Produces a 4 panel plot that gives an overview of the data set prior to any processing
#'
#' @description
#' The four plots produced are 1) log concentration versus log discharge, 2) log concentration versus time
#' 3) a boxplot of log concentration by month, and 
#' 4) a side-by-side boxplot of the sampled discharges and all daily discharges. 
#' To save space, the graphic is labeled only at the top of the 4 graph display. 
#' 
#' Although there are a lot of optional arguments to this function, most are set to a logical default. 
#'
#' @param eList named list with at least Daily, Sample, and INFO dataframes
#' @param qUnit object of qUnit class \code{\link{printqUnitCheatSheet}}, or numeric represented the short code, or character representing the descriptive name.
#' @param cex.main magnification to be used for main titles relative to the current setting of cex
#' @param logScaleConc logical if TRUE y in concentration graphs plotted in log axis. Default is TRUE.
#' @param logScaleQ logical if TRUE y in streamflow graphs plotted in log axis. Default is TRUE.
#' @param USGSstyle logical use USGSwsGraph package for USGS style
#' @keywords graphics water-quality statistics
#' @seealso \code{\link{plotConcQ}}, \code{\link{boxConcMonth}}, \code{\link{plotConcTime}}, \code{\link{boxQTwice}}
#' @export
#' @examples
#' eList <- Choptank_eList
#' # Water year:
#' multiPlotDataOverview(eList, qUnit=1)
#' # Graphs consisting of Jun-Aug
#' eList <- setPA(eList, paStart=6,paLong=3)
#' multiPlotDataOverview(eList, qUnit=1) 
#' 
#' eList <- setPA(eList, paStart=10,paLong=12)
#' library(smwrGraphs)
#' setPDF(basename="multiPlotDataOverview",layout="landscape")
#' multiPlotDataOverview(eList, qUnit=2,USGSstyle=TRUE) 
#' graphics.off()
multiPlotDataOverview<-function (eList, qUnit = 2,cex.main=1.2,
                                 logScaleConc=TRUE, logScaleQ=TRUE,
                                 USGSstyle=FALSE){
  
  localINFO <- getInfo(eList)
  
  if(sum(c("paStart","paLong") %in% names(localINFO)) == 2){
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart  
  } else {
    paLong <- 12
    paStart <- 10
  }
  
  title2<-if(paLong==12) "" else setSeasonLabelByUser(paStartInput=paStart,paLongInput=paLong)
  title<-paste(localINFO$shortName,"\n",localINFO$paramShortName)
  
  if(USGSstyle){
    layoutResponse <- setLayout(num.rows=2,num.cols = 3, 
                                num.graphs = 5, explanation = list(grid=c(6)))
    graph1 <- setGraph(1, layoutResponse)
    plotConcQ(eList, qUnit = qUnit, printTitle = FALSE,rmSciX=TRUE,tinyPlot = TRUE,
              logScale=logScaleConc,USGSstyle=USGSstyle,legend=FALSE, 
              margin=graph1)
#     if("" != title2){
#       title <- paste(title, title2, sep="\n")   
#     }
#     addTitle(Main = title)
    graph2 <- setGraph(2, layoutResponse)
    concTimeOut <- plotConcTime(eList, printTitle = FALSE, tinyPlot = TRUE,
                 logScale=logScaleConc,USGSstyle=USGSstyle,legend=TRUE, 
                 margin=graph2)
    
    
    graph3 <- setGraph(3, layoutResponse)
    reportGraph(paste("\n",title ))
    graph4 <- setGraph(4, layoutResponse)
    boxConcOut <- boxConcMonth(eList, printTitle = FALSE, tinyPlot = TRUE,
                 logScale=logScaleConc,USGSstyle=USGSstyle, margin=graph4)
#     addCaption(paste("\n",title))
    graph5 <- setGraph(5, layoutResponse)
    boxOut <- boxQTwice(eList, printTitle = FALSE, qUnit = qUnit, tinyPlot = TRUE,
              logScale=logScaleQ,USGSstyle=USGSstyle, margin=graph5)
    graphExplain <- setGraph("explanation", layoutResponse)
    addExplanation(boxOut)

  } else {
    par(mfcol=c(2,2),oma=c(0,2.4,4.5,2.4),tcl=0.5)
    plotConcQ(eList, qUnit = qUnit, tinyPlot = TRUE, printTitle = FALSE,rmSciX=TRUE,
              logScale=logScaleConc,USGSstyle=USGSstyle)
    boxConcMonth(eList, printTitle = FALSE, tinyPlot=TRUE,
                 logScale=logScaleConc,USGSstyle=USGSstyle)
    plotConcTime(eList, printTitle = FALSE, tinyPlot = TRUE,
                 logScale=logScaleConc,USGSstyle=USGSstyle)
    boxQTwice(eList, printTitle = FALSE, qUnit = qUnit, tinyPlot=TRUE,
              logScale=logScaleQ,USGSstyle=USGSstyle)
   
    
    if("" == title2){
      mtext(title,cex=cex.main,outer=TRUE,font=2)
    } else {
      title <- paste(title, title2, sep="\n")
      mtext(title, cex = cex.main*.75, outer = TRUE, font = 2)    
    }
    par(mfcol=c(1,1),oma=c(0,0,0,0))
  }

}