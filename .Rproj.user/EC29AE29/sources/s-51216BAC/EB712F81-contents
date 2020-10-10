#' Function for creating a numeric vector showing the smallest distance between the
#' feature in the primary sf object and all features in the second object
#' @param x the primary sf object
#' @param y the second sf object
#' @keywords eucledian distance, sf, spatial, polygons
#' @export
#' @examples
#' library(sf)
#' records <- readxl::read_excel(system.file("data/records.xlsx", package = "ALERC")) %>%
#'   st_as_sf(coords = c("Easting", "Northing"), crs = 27700)
#' sssi <- st_read(system.file("data/SSSI.shp", package = "ALERC")) %>%
#'   st_set_crs(27700)
#' records$DistFromSSSI <- st_min_dist(records, sssi)
#'
#'
st_min_dist <- function(x, y){
  library(sf)
  library(dplyr)
index <- st_nearest_feature(x, y)
y <- y %>% slice(index)
output <- round(as.numeric(st_distance(x, y, by_element = TRUE)), 2)
}



