#' Function for truncating a text field designed to aid labelling a map
#' This function returns a character string
#' @param string the string you want truncating
#' @param 
#' @keywords mapping, labelling, truncate, text string, character string
#' @export
#' @examples
#' sites_lab <- sites_lab %>% mutate(latitude = as.numeric(st_coordinates(sites_lab)[,1]), 
#'                                   longitude = as.numeric(st_coordinates(sites_lab)[,2]), 
#'                                   SiteNam = as.character(SiteNam), 
#'                                   SiteNam = str_replace_all(SiteNam, " and ", " & "),
#'                                   SiteNamShort = case_when(nchar(SiteNam) > 40 ~ trunc_labs(SiteNam), 
#'                                                            TRUE ~ SiteNam))

trunc_labs <- function(string){
  library(stringr)
  
  string <- as.character(string)
  labs <- strsplit(string, " ")
  labs <- lapply(labs,function(x) ifelse(nchar(x)>3, str_remove_all(x, "[aeiou]"), x))
  labs <- paste(labs)
  labs <- str_replace_all(labs, 'c[(]', "")
  labs <- str_replace_all(labs, '[)]$', "")
  labs <- str_replace_all(labs, '["]', "")
  labs <- str_replace_all(labs, '[,]', "")
  return(labs)
}