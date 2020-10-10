#' OS Grid Ref to Eastings and Northings Function
#'
#' This function creates a data frame containing the easting and northing coordinates for the SW corner of the grid reference.
#' @param gridreference The grid reference to be converted. Can be any resolution.
#' @keywords Grid references
#' @export
#' @examples
#' OS2EN()

OS2EN <- function(gridreference){

  library(tidyverse)
  options(warn=-1)

gridreflookup <- tibble(code = c("SV", "SW", "SX", "SY", "SZ", "TV",
                                 "SQ", "SR", "SS", "ST", "SU", "TQ", "TR",
                                 "SM", "SN", "SO", "SP", "TL", "TM",
                                 "SG", "SH", "SJ", "SK", "TF", "TG",
                                 "SB", "SC", "SD", "SE", "TA",
                                 "NW", "NX", "NY", "NZ", "OV",
                                 "NQ", "NR", "NS", "NT", "NU", "OQ"),
                        easting = c(0:5, 0:6, 1:6, 1:6, 1:5, 1:5, 0:5),
                        northing = c(rep(0, 6), rep(1, 7), rep(2, 6),
                                           rep(3, 6), rep(4, 5), rep(5, 5), rep(6, 6))
                        )

df <- as_tibble(gridreference)
gridreftable <- df %>% mutate(gridref = as.character(gridreference),
                              gridref = gsub(" ", "", gridreference),
                       code = substr(gridref, 1, 2),
                       code = toupper(code),
                       numbers = substr(gridref, 3, 20),
                       halflength = nchar(numbers)/2,
                       easting2 = case_when(halflength == 1.5 ~ paste0(substr(numbers, 1, 1),
                                        case_when(substr(numbers, 3, 3) %in% c("A", "B", "C", "D", "E") ~ "0",
                                                  substr(numbers, 3, 3) %in% c("F", "G", "H", "I", "J") ~ "2",
                                                  substr(numbers, 3, 3) %in% c("K", "L", "M", "N", "P") ~ "4",
                                                  substr(numbers, 3, 3) %in% c("Q", "R", "S", "T", "U") ~ "6",
                                                  substr(numbers, 3, 3) %in% c("V", "W", "X", "Y", "Z") ~ "8",)),
                                        halflength != 1.5 ~ substr(numbers, 1, halflength)),
                       northing2 = case_when(halflength == 1.5 ~ paste0(substr(numbers, 2, 2),
                                        case_when(substr(numbers, 3, 3) %in% c("A", "F", "K", "Q", "V") ~ "0",
                                                  substr(numbers, 3, 3) %in% c("B", "G", "L", "R", "W") ~ "2",
                                                  substr(numbers, 3, 3) %in% c("C", "H", "M", "S", "X") ~ "4",
                                                  substr(numbers, 3, 3) %in% c("D", "I", "N", "T", "Y") ~ "6",
                                                  substr(numbers, 3, 3) %in% c("E", "J", "P", "U", "Z") ~ "8")),
                                         halflength != 1.5 ~ substr(numbers, halflength + 1, 10)),
                       zeros = "0000") %>%
                       inner_join(gridreflookup, by = "code") %>%
                       mutate(finaleasting = as.numeric(substr(paste0(easting,easting2,zeros), 1, 6)),
                              finalnorthing = as.numeric(substr(paste0(northing,northing2,zeros), 1, 6))) %>%
                       dplyr::select(finaleasting, finalnorthing) %>%
                       dplyr::select(Easting = finaleasting, Northing = finalnorthing)

gridreftable

}


