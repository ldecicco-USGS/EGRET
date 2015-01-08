#' Makes 15 graphs of streamflow statistics on a single page
#'
#' Part of flowHistory system.
#'
#' @param yearStart A numeric value for year in which the graph should start, default is NA, which indicates that the graph should start with first annual value
#' @param yearEnd A numeric value for year in which the graph should end, default is NA, which indicates that the graph should end with last annual value
#' @param eList named list with at least the Daily and INFO dataframes
#' @param USGSstyle logical use USGSwsGraph package for USGS style
#' @keywords graphics streamflow statistics
#' @export
#' @seealso \code{\link{plot1of15}}
#' @examples
#' eList <- Choptank_eList
#' \dontrun{
#' pdf("plot15.pdf",height=10,width=8)
#' plot15(eList, yearStart=1990,yearEnd=2000)
#' dev.off()
#' 
#' setPDF(basename="plot15_USGS", layout="portrait")
#' plot15(eList, yearStart=1990,yearEnd=2000,USGSstyle=TRUE)
#' graphics.off()
#' }
plot15<-function(eList, yearStart,yearEnd,USGSstyle=FALSE){
#   plotName<-paste(savePath,"plot15.",localINFO$staAbbrev,".ps",sep="")
#   postscript(file=plotName,width=8,height=10,horizontal=FALSE,family="Helvetica")
  
  localINFO <- getInfo(eList)
  qf<-86/localINFO$drainSqKm
  
  if(USGSstyle){
    layoutResponse <- setLayout(num.rows=5,num.cols = 3, 
                                num.graphs = 15)
    graph1 <- setGraph(1, layoutResponse)
    eList<-setPA(eList, 10,12)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,
                           qf,istat=2, USGSstyle=TRUE, 
                           margin=graph1)
    
    addTitle(Main="7-day minimum",Justification = "center" )
    addAxisLabels(which="left",current=plotStuff, title="Annual values\nin millimeters per day")
    
    graph2 <- setGraph(2, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=5, USGSstyle=TRUE, margin=graph2)
    addTitle(Main="Mean",Justification = "center")
#     mtext(localINFO$shortName,cex=1.0,font=1,side=3,line=4)

    graph3 <- setGraph(3, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=8, USGSstyle=TRUE, margin=graph3)
    addTitle(Main="1-day maximum",Justification = "center")
    
    # fall season
    eList<-setPA(eList,9,3)
    graph4 <- setGraph(4, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=2, USGSstyle=TRUE, margin=graph4)
    addAxisLabels(which="left",current=plotStuff, title="Fall season values\nin millimeters per day")
    
    graph5 <- setGraph(5, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=5, USGSstyle=TRUE,legend=TRUE, margin=graph5)
    graph6 <- setGraph(6, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=8, USGSstyle=TRUE, margin=graph6)
    
    # winter season
    eList<-setPA(eList, 12,3)
    graph7 <- setGraph(7, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=2, USGSstyle=TRUE, margin=graph7)
    addAxisLabels(which="left",current=plotStuff, title="Winter season values\nin millimeters per day")
    graph8 <- setGraph(8, layoutResponse)    
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=5, USGSstyle=TRUE, margin=graph8)
    graph9 <- setGraph(9, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=8, USGSstyle=TRUE, margin=graph9)
    
    # spring season
    eList <- setPA(eList,3,3)
    graph10 <- setGraph(10, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=2, USGSstyle=TRUE, margin=graph10)
    addAxisLabels(which="left",current=plotStuff, title="Spring season values\nin millimeters per day")
    graph11 <- setGraph(11, layoutResponse)    
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=5, USGSstyle=TRUE, margin=graph11)
    graph12 <- setGraph(12, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=8, USGSstyle=TRUE, margin=graph12)
    
    caption<-paste("\nStreamflow statistics (circles) in units of millimeters per day, annual values and seasonal values\nFall (Sept., Oct., and Nov.), Winter (Dec., Jan., and Feb.), Spring (Mar., Apr., and May), and Summer (June, July, and Aug.)\nand locally weighted scatterplot smooth (solid curve) for ",
               localINFO$shortName," for ",yearStart," - ",yearEnd,".",sep="")

    # summer season
    graph13 <- setGraph(13, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=2,isBottom=TRUE, USGSstyle=TRUE, margin=graph13)
    addAxisLabels(which="left",current=plotStuff, title="Summer season values\nin millimeters per day")
    addCaption(caption=caption)    
    graph14 <- setGraph(14, layoutResponse)    
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=5,isBottom=TRUE, USGSstyle=TRUE, margin=graph14)
    graph15 <- setGraph(15, layoutResponse)
    plotStuff <- plot1of15(eList, yearStart,yearEnd,qf,istat=8,isBottom=TRUE, USGSstyle=TRUE, margin=graph15)

#     mtext(caption,side=1,outer=TRUE,line=7,adj=0,font=1,cex=0.7)
    
  } else {
    par(mfrow=c(5,3),cex=0.6,oma=c(10,8,10,4),mar=c(1,4,1,1))
    
    eList<-setPA(eList, 10,12)
    plot1of15(eList, yearStart,yearEnd,qf,istat=2)
    mtext("7-day minimum",cex=0.8,font=1,side=3,line=1)
    mtext("Annual values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
    plot1of15(eList, yearStart,yearEnd,qf,istat=5)
    mtext("Mean",cex=0.8,font=1,side=3,line=1)
    mtext(localINFO$shortName,cex=1.0,font=1,side=3,line=4)
    plot1of15(eList, yearStart,yearEnd,qf,istat=8)
    mtext("1-day maximum",cex=0.8,font=1,side=3,line=1)
    # fall season
    eList<-setPA(eList,9,3)
    plot1of15(eList, yearStart,yearEnd,qf,istat=2)
    mtext("Fall season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
    plot1of15(eList, yearStart,yearEnd,qf,istat=5)
    plot1of15(eList, yearStart,yearEnd,qf,istat=8)
    # winter season
    eList<-setPA(eList, 12,3)
    plot1of15(eList, yearStart,yearEnd,qf,istat=2)
    mtext("Winter season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
    plot1of15(eList, yearStart,yearEnd,qf,istat=5,)
    plot1of15(eList, yearStart,yearEnd,qf,istat=8,)
    # spring season
    eList <- setPA(eList,3,3)
    plot1of15(eList, yearStart,yearEnd,qf,istat=2)
    mtext("Spring season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
    plot1of15(eList, yearStart,yearEnd,qf,istat=5)
    plot1of15(eList, yearStart,yearEnd,qf,istat=8)
    # summer season
    eList <- setPA(eList, 6,3)
    plot1of15(eList, yearStart,yearEnd,qf,istat=2,isBottom=TRUE)
    mtext("Summer season values,\nin mm/day",side=2,cex=0.8,font=1,line=4)
    plot1of15(eList, yearStart,yearEnd,qf,istat=5,isBottom=TRUE)
    plot1of15(eList, yearStart,yearEnd,qf,istat=8,isBottom=TRUE)
    caption<-paste("\nStreamflow statistics (circles) in units of millimeters per day, annual values and seasonal values\nFall (Sept., Oct., and Nov.), Winter (Dec., Jan., and Feb.), Spring (Mar., Apr., and May), and Summer (June, July, and Aug.)\nand locally weighted scatterplot smooth (solid curve) for ",
                   localINFO$shortName," for ",yearStart," - ",yearEnd,".",sep="")
    mtext(caption,side=1,outer=TRUE,line=7,adj=0,font=1,cex=0.7)
  }
#   dev.off()
}
