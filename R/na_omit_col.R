#' Remove rows based on NA values in one column
#'
#' This function removes rows from a dataframe when one column contains NAs
#' @param data the data frame
#' @param desiredCols the heading of the specific column
#' @keywords remove rows with na in column
#' @export
#' @examples df1 <- na_omit_col(df, "ID")
#' na_omit_col()


na_omit_col <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
