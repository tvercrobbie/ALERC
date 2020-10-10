#' Function for creating a numeric vector showing the smallest distance between the
#' polygon in the primary sf object and all polygons in the second object
#' @param x the primary sf object
#' @param y the second sf object
#' @keywords eucledian distance, sf, spatial, polygons
#' @export
#' @examples
#' records <- read.xlsx("records.xlsx")
#' lws <- st_read("G:/Data/Data to Use/Sites/Berks/LWS/GIS data/2019/Berkshire Local Wildlife Sites April 2019.TAB")
#' records <- OS2POLY(records)
#' records$distance <- edist(records, lws)
#'
edist <- function(x, y){
  library(sf)
  library(dplyr)
index <- st_nearest_feature(x, y)
y <- y %>% slice(index)
output <- as.numeric(st_distance(x, y, by_element = TRUE))
}

