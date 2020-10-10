#' TVERC's custom ggplot theme.
#' @keywords ggplot, theme
#' @export
#' @examples
#' library(sf)
#' myshape <- st_read(system.file("data/BerksOxon.shp", package = "ALERC"))
#' ggplot(myshape, aes(col = County)) +
#' geom_sf() +
#' coord_sf(datum = 27700) +
#' theme_tverc()

theme_tverc <- function(){
  theme_bw() %+replace%
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}
