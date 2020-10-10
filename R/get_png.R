#' Function for reading a png as a raster
#' @param filename the  filename of the png file
#' @keywords png, mapping, ggplot
#' @export
#' @examples
#' library(sf)
#' logo <- get_png(system.file("data/TVERC_logo.png", package = "ALERC"))
#' myshape <- st_read(system.file("data/BerksOxon.shp", package = "ALERC"))
#' library(ggplot2)
#' ggplot(myshape) +
#'   geom_sf() +
#'   coord_sf(datum = 27700) +
#'   annotation_custom(logo, xmin = 470000, xmax = 504000, ymin = 240000, ymax = Inf) +
#'   theme_tverc()

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}
