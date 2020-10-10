#' Eastings and Northings to OS Grid Ref function
#'
#' This function converts eastings and northings to an OS grid ref of a specified length.
#' @param easting The eastings of the point to be converted.
#' @param northing The northings of the point to be converted.
#' @param resolution The resolution of the grid reference to be produced.
#' @keywords Grid references
#' @export
#' @examples
#' en2os()

en2os <- function(easting, northing, resolution){
  
  #options(warn=-1)
  squarecodes <- c("SV", "SW", "SX", "SY", "SZ", "TV",
                   "SQ", "SR", "SS", "ST", "SU", "TQ", "TR",
                   "SM", "SN", "SO", "SP", "TL", "TM",
                   "SG", "SH", "SJ", "SK", "TF", "TG",
                   "SB", "SC", "SD", "SE", "TA",
                   "NW", "NX", "NY", "NZ", "OV",
                   "NQ", "NR", "NS", "NT", "NU", "OQ")
  
  squareeasting <- c(0:5, 0:6, 1:6, 1:6, 1:5, 1:5, 0:5)
  
  squarenorthing <- c(rep(0, 6), rep(1, 7), rep(2, 6), rep(3, 6), rep(4, 5), rep(5, 5), rep(6, 6))
  
  gridreflookup <- data.frame(squarecodes, squareeasting, squarenorthing)
  colnames(gridreflookup) <- c("squarecode", "squareeasting", "squarenorthing")
  gridreflookup$numbercode <- paste0(squareeasting, squarenorthing)
  
  eastingin <- easting
  northingin <- northing
  endf <- data.frame(eastingin, northingin)
  endf$numbercode <- paste0(substr(eastingin, 1, 1),substr(northingin, 1, 1))
  endf <- inner_join(endf, gridreflookup, by = "numbercode")
  
  endf$eastingnumbers <- substr(eastingin, 2, (resolution/2 + 1))
  endf$northingnumbers <- substr(northingin, 2, (resolution/2 + 1))
  
  output <- paste0(endf$squarecode, endf$eastingnumbers, endf$northingnumbers)
  output
  
  
}


