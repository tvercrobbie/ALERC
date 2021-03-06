% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trunc_labs.R
\name{trunc_labs}
\alias{trunc_labs}
\title{Function for truncating a text field designed to aid labelling a map
This function returns a character string}
\usage{
trunc_labs(string)
}
\arguments{
\item{string}{the string you want truncating}
}
\description{
Function for truncating a text field designed to aid labelling a map
This function returns a character string
}
\examples{
sites_lab <- sites_lab \%>\% mutate(latitude = as.numeric(st_coordinates(sites_lab)[,1]), 
                                  longitude = as.numeric(st_coordinates(sites_lab)[,2]), 
                                  SiteNam = as.character(SiteNam), 
                                  SiteNam = str_replace_all(SiteNam, " and ", " & "),
                                  SiteNamShort = case_when(nchar(SiteNam) > 40 ~ trunc_labs(SiteNam), 
                                                           TRUE ~ SiteNam))
}
\keyword{character}
\keyword{labelling,}
\keyword{mapping,}
\keyword{string}
\keyword{string,}
\keyword{text}
\keyword{truncate,}
