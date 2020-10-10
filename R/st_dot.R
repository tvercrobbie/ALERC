#' Function for creating an sf point object to resemble dotting within a polygon for plotting on a map.
#' This function returns an sf point object.
#' Eventually the package {ggpattern} will render this function useless, but the functionality is not yet there.
#' @param x the sf object containing polygons that you want dotted.
#' @param custom_bbox the option for a custom bounding box to standardise density in a repetitive mapping process. If blank, the bbox of the sf object will be used.
#' @param den the density of dots.
#' @keywords sf, dot, mapping, symbology, ggplot
#' @export
#' @examples
#' library(sf)
#' myshape <- st_read(system.file("data/TwoCounties_region.shp", package = "ALERC"))
#' bboxplus <- st_bbox(st_buffer(myshape, 100))
#' dots <- st_dot(myshape, den = 200, custom_bbox = bboxplus)
#' library(ggplot2)
#' ggplot(myshape, aes(col = COUNTYNAME)) +
#' geom_sf() +
#' geom_sf(data = dots, aes(col = COUNTYNAME)) +
#' coord_sf(datum = 27700)


st_dot <- function(x, custom_bbox = NULL, den = 4000){
  library(sf)
  library(dplyr)
  if(is.null(custom_bbox)){
    dots = st_sample(st_as_sfc(st_bbox(x)), den, type = "hexagonal") %>%
      st_sf() %>%
      #st_transform(27700) %>%
      rename("WKT" = geometry)
  }else{
    dots = st_sample(st_as_sfc(custom_bbox), den, type = "hexagonal") %>%
      st_sf() %>%
      #st_transform(27700) %>%
      rename("WKT" = geometry)
  }
  dots = st_intersection(dots, x)
  return(dots)
}
