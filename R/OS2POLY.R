#' Function for creating a polygon from a grid reference, with the size of the polygon dependant on the precision of the grid reference
#'
#' This function creates an sf polygon object, .
#' @param df The data frame, must include a column containing grid references called "GridRef"
#' @keywords Grid references
#' @export
#' @examples
#' OS2POLY()

OS2POLY <- function(df, col = "GridRef"){
library(TVERC)
library(tidyverse)

df <- df %>%
  rename(GridRef = col) %>%
  mutate(GridRef =  gsub(" ", "", GridRef, fixed = TRUE)) %>%
  filter(grepl("^[a-zA-Z]{2}[0-9]{4,12}$", GridRef) | grepl("^[a-zA-Z]{2}[0-9]{2}[a-zA-Z]{1}$", GridRef))
x <- OS2EN(df$GridRef)

df$SWE = x$Easting
df$SWN = x$Northing

df <- df %>%
           mutate(l = as.numeric(nchar(GridRef)),
                  SEE = case_when(l == 5 ~ SWE + 2000,
                                  l == 6 ~ SWE + 1000,
                                  l == 8 ~ SWE + 100,
                                  l == 10 ~ SWE + 10,
                                  l == 12 ~ SWE + 1,
                                  TRUE ~ SWE),
                  SEN = SWN,
                  NWN = case_when(l == 5 ~ SWN + 2000,
                                  l == 6 ~ SWN + 1000,
                                  l == 8 ~ SWN + 100,
                                  l == 10 ~ SWN + 10,
                                  l == 12 ~ SWN + 1,
                                  TRUE ~ SWN),
                  NWE = SWE,
                  NEE = case_when(l == 5 ~ SWE + 2000,
                                  l == 6 ~ SWE + 1000,
                                  l == 8 ~ SWE + 100,
                                  l == 10 ~ SWE + 10,
                                  l == 12 ~ SWE + 1,
                                  TRUE ~ SWE),
                  NEN = case_when(l == 5 ~ SWN + 2000,
                                  l == 6 ~ SWN + 1000,
                                  l == 8 ~ SWN + 100,
                                  l == 10 ~ SWN + 10,
                                  l == 12 ~ SWN + 1,
                                  TRUE ~ SWN),
                  geometry = paste0("POLYGON ((",
                                  as.integer(SWE), " ", as.integer(SWN), ", ",
                                  as.integer(NWE), " ", as.integer(NWN), ", ",
                                  as.integer(NEE), " ", as.integer(NEN), ", ",
                                  as.integer(SEE), " ", as.integer(SEN), ", ",
                                  as.integer(SWE), " ", as.integer(SWN),
                                  "))",
                                  sep = "")
                   )
drop.cols <- c("SWE", "SWN", "l", "SEE", "SEN", "NWN", "NWE", "NEE", "NEN")
df <- df %>% dplyr::select(-one_of(drop.cols))

output <- st_as_sf(df, crs = 27700, wkt = "geometry")
output
}
