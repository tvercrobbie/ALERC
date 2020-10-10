#' Function to write out a shapefile with correct .prj
#'
#' This function creates an sf polygon object, .
#' @param data The data to be written
#' @param file The path and name of the shapefile to be written, without the .shp
#' @keywords Shapefile
#' @export
#' @examples
#' write_shapefile()


write_shapefile <- function(data, file){
  library(sf)
  st_write(data, paste0(file, ".shp"), delete_layer = TRUE)
  fileConn <- paste0(file, ".prj")
  writeLines('PROJCS["OSGB_1936_British_National_Grid",GEOGCS["GCS_OSGB 1936",DATUM["D_OSGB_1936",SPHEROID["Airy_1830",6377563.396,299.3249646]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["Meter",1]]', fileConn)
  closeAllConnections()
}
