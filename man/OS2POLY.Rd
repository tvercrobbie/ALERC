% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OS2POLY.R
\name{OS2POLY}
\alias{OS2POLY}
\title{Function for creating a polygon from a grid reference, with the size of the polygon dependant on the precision of the grid reference}
\usage{
OS2POLY(df, col = "GridRef")
}
\arguments{
\item{df}{The data frame, must include a column containing grid references called "GridRef"}
}
\description{
This function creates an sf polygon object, .
}
\examples{
OS2POLY()
}
\keyword{Grid}
\keyword{references}
