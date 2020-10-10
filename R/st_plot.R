#' Function for plotting sf objects without having to specific st_geometry
#' This function plots an sf object.
#' @param sf the  sf object 
#' @param add default is false, specify add = T if you want the object to be added to the current plot
#' @keywords sf, plot
#' @export
#' @examples
#' st_plot()
#' 

st_plot <- function(sf, add = F, col = "black"){
  if(add==T){
    plot(st_geometry(sf), add = T, col = col)
  }else{
      plot(st_geometry(sf), col = col)
    }
  }
  