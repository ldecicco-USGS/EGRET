#' makeRandomResiduals
#'
#' Create randomized residuals for censored values.
#' 
#' @param eList named list with at least the Daily and INFO dataframes
#' @export
#' @examples
#' eList <- Choptank_eList
#' eList <- makeRandomResiduals(eList)
makeRandomResiduals <- function(eList){
  
	localSample <- eList$Sample
	
	numSamples<-length(localSample$Uncen)
	
	a <- ifelse(localSample$Uncen==0 & !is.na(localSample$ConcLow),
	            log(localSample$ConcLow)-localSample$yHat,-Inf)
	
	b <- ifelse(localSample$Uncen==1, +Inf, 
	            log(localSample$ConcHigh) - localSample$yHat)
	
	mean <- ifelse(localSample$Uncen==1, 
	               log(localSample$ConcHigh) - localSample$yHat,0)
	
	sd <- ifelse(localSample$Uncen==1, 
	             0, localSample$SE)
	
	localSample$rResid <- rtruncnorm(numSamples,a,b,mean,sd)
	
	eList <- as.egret(eList$INFO, eList$Daily, localSample, eList$surfaces)
	
	return(eList)
}