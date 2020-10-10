#' Function for creating a polygon from a grid reference, with the size of the polygon dependant on the precision of the grid reference
#'
#' This function creates an sf polygon object, .
#' @param df The data frame, must include a column containing grid references called "GridRef"
#' @keywords Grid references
#' @export
#' @examples
#' OS2POINT()

OS2POINT <- function(df, col = "GridRef"){
  library(TVERC)
  library(tidyverse)
  library(sf)

  df <- df %>%
    rename(GridRef = col) %>%
    mutate(GridRef =  gsub(" ", "", GridRef, fixed = TRUE)) %>%
    filter(grepl("^[a-zA-Z]{2}[0-9]{4,12}$", GridRef) | grepl("^[a-zA-Z]{2}[0-9]{2}[a-zA-Z]{1}$", GridRef))

   x <- OS2EN(df$GridRef)

   df$Easting <- x$Easting
   df$Northing <- x$Northing

  df <- df %>%
    mutate(
      # Easting = x$Easting,
      #      Northing = x$Northing,
           geometry = paste("POINT (",
                            Easting, " ", Northing,
                            ")",
                       sep = "")
    )

  output <- st_as_sf(df, crs = 27700, wkt = "geometry")
  output
}


