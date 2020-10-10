#' Function for finding a centroid and ensuring that it is within the polygon for irregularly shaped polygons
#' This function returns an sf point object.
#' @param poly the  sf object containing polygons that you want centroids for
#' @param 
#' @keywords sf, centroid, within polygon
#' @export
#' @examples
#' sites_lab <- st_centroid_within_poly(sites_crop) 
#' 

st_centroid_within_poly <- function (poly) {
  
  library(sf)
  
  # check if centroid is in polygon
  ctrd <- st_centroid(poly, of_largest_polygon = TRUE)
  in_poly <- diag(st_within(ctrd, poly, sparse = F))
  
  # replace geometries that are not within polygon with st_point_on_surface()
  st_geometry(ctrd[!in_poly,]) <- st_geometry(st_point_on_surface(poly[!in_poly,]))
  
  ctrd 
}
