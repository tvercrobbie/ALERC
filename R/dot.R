#' Function for creating an sf point object to resemble dotting within a polygon for plotting on a map.
#' This function returns an sf point object.
#' Eventually the package {ggpattern} will render this function useless, but the functionality is not yet there.
#' @param sf_df the sf object containing polygons that you want dotted
#' @param sf_bbox a
#' @keywords sf, dot, mapping
#' @export
#' @examples
#' dots <- dot(sf_df, sf_bbox)
#'

dot <- function(x, bbox = NULL, den = 4000){
  library(sf)
  library(dplyr)
  if(is.null(bbox)){
    dots = st_sample(st_as_sfc(st_bbox(x)), den, type = "hexagonal") %>%
      st_sf() %>%
      #st_transform(27700) %>%
      rename("WKT" = geometry)
  }else{
  dots = st_sample(st_as_sfc(bbox), den, type = "hexagonal") %>%
    st_sf() %>%
    #st_transform(27700) %>%
    rename("WKT" = geometry)
  }
  dots = st_intersection(dots, x)
  return(dots)
}
