#' Function for reading a png as a raster
#' @param filename the  filename of the png file
#' @keywords png, mapping, ggplot
#' @export
#' @examples
#' logo <- get_png("G:/Logo.png")
#'

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}