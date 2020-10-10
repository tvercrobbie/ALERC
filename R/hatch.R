#' Function for creating an sf lines object to resemble hatching within a polygon for plotting on a map
#' This function returns an sf linestring or multilinestring object.
#' @param x the  sf object containing polygons that you want hatching
#' @param pattern horizontal, vertical, left2right, right2left - determines the hatching angle
#' @keywords sf, hatch, mapping
#' @export
#' @examples
#' right_hatches <- hatch(sites_crop, right2left, sf_list$Buffer")
#'

hatch <- function(x, pattern, bbox) {
  library(sf)
  ex = list(horizontal = c(1, 2),
            vertical = c(1, 4),
            left2right = c(2, 4),
            right2left = c(1, 3))
  radius <- bbox[3] - bbox[1]
  den <- radius / 62.5
  fillgrid = st_make_grid(x, cellsize = den)
  endsf = lapply(1:length(fillgrid), function(j)
    st_linestring(st_coordinates(fillgrid[j])[ex[[pattern]], 1:2]))
  endsf = st_sfc(endsf, crs = st_crs(27700))
  endsf <- data.frame(matrix(unlist(endsf), nrow=length(endsf), byrow=T))
  endsf <- as.data.frame(paste0("LINESTRING (", endsf$X1, " ", endsf$X3, ",", endsf$X2, " ", endsf$X4, ")"))
  colnames(endsf) <- "WKT"
  endsf <- st_as_sf(endsf, crs = 27700, wkt = "WKT")
  endsf = st_intersection(endsf, x)
  endsf = endsf[st_geometry_type(endsf$WKT)
                %in% c("LINESTRING", "MULTILINESTRING"),]
  return(endsf)
}
