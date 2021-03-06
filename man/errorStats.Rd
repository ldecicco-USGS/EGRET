% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/errorStats.R
\name{errorStats}
\alias{errorStats}
\title{Error statistics}
\usage{
errorStats(eList)
}
\arguments{
\item{eList}{named list with at least the Daily, Sample, and INFO dataframes}
}
\value{
erStats a numeric vector consisting of the following statistics
RsqLC the R squared value for predictions of ln(Concentration)
RsqLF the R squared value for predictions of ln(Flux)
rmse the root mean squared error for ln(Concentration), same value would apply for Flux
sepPercent the standard error of prediction for Concentration, expressed in percent
    same value would apply for Flux
}
\description{
This function takes a fitted WRTDS model and computes error statistics
the residuals used here are cross-validation residuals, 
which will be slightly larger than regular regression residuals
in the case of censored data, the residuals are computed from random residuals
computed from makeAugmentedSample(), the function returns a list of error statistics
and also prints them to the console
}
\examples{
eList <- Choptank_eList
errorStats(eList)
}
