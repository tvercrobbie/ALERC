#' Reads in .csv, .xls amd .xlsx files to a data frame
#'
#' This function will read in any excel file in .csv, .xls or .xlsx.
#' @param file The file path, use file.choose()
#' @keywords Import spreadsheet
#' @export
#' @examples
#' read.excel()

read.excel <- function(file){
library(openxlsx)
library(readxl)

output <- if (endsWith(file, ".csv")) {
  read.csv(file, stringsAsFactors = F)
} else {
  read_excel(file)
}
}


