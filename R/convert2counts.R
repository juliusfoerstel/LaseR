#' Convert to Counts
#'
#' This function converts all voltages to counts.
#' @param data Input data
#'     cups On which cup the Ions were measured.  Default is no entry needed.
#' @keywords volts, counts
#' @export
#' @examples
#' convert2counts(data)

convert2counts <- function(data,cups = get.cups()){
  nCups <- length(cups)
  is.volt <- c(F,F,!(cups=="C(C)"))
  
  data[,is.volt] <- data[,is.volt]*res11
  
  return(data)
}