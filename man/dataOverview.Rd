% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataOverview.r
\name{dataOverview}
\alias{dataOverview}
\title{Data Overview for WRTDS}
\usage{
dataOverview(Daily, Sample)
}
\arguments{
\item{Daily}{dataframe}

\item{Sample}{dataframe}
}
\description{
Gives a summary of data to be used for WRTDS analysis
}
\examples{
eList <- Choptank_eList
exDaily <- getDaily(eList)
exSample <- getSample(eList)
dataOverview(Daily = exDaily, Sample = exSample)
}
\seealso{
\code{\link{mergeReport}}
}
\keyword{USGS}
\keyword{WRTDS}
\keyword{data}
\keyword{import}
