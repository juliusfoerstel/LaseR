#' Clear Outliers
#'
#' This function deletes the entries which deviate more than a specific sigma range from the mean.
#' @param x Input data
#'     a Sigma range
#' @keywords outlier, sigma, range
#' @export
#' @examples
#' clear.outliers(x = data, a = 2)


clear.outliers <- function(x,a=2){
  valid <-  abs(x-mean(x,na.rm = T)) < a*sd(x,na.rm = T)
  x[!valid] <- NA
  return(x)
}