#' Add Ratios
#'
#' This function adds measured and activity ratios to your data frame.
#' @param data Data frame with original data.
#' @keywords ratios
#' @export
#' @examples
#' add.ratios(data)

add.ratios <- function(data){
  data$mr08 <- data$X1.230Th/data$X1.238U
  data$ar08 <- data$mr08 * lambda230/lambda238
  
  data$mr48 <- data$X2.234U/data$X2.238U
  data$ar48 <- data$mr48 * lambda234/lambda238
  
  data$mr02 <- data$X1.230Th/data$X2.232Th
  data$ar02 <- data$mr02 * lambda230/lambda232
  
  data$mr28 <- data$X2.232Th/data$X1.238U
  data$ar28 <- data$mr28 * lambda232/lambda238
  
  return(data)
}