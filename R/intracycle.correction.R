#' Intracycle correction
#'
#' This function corrects differences between the cycles by correcting for 238U values.
#' @param data Data input
#' @keywords intracycle
#' @export
#' @examples
#' intracycle.correction(data)

intracycle.correction <- function(data){
  colnam <- colnames(data)
  
  first238 <- grepl('X1.238',colnam)
  second238 <- grepl('X2.238',colnam)
  first <- grepl('X1.',colnam)
  second <- grepl('X2.',colnam)
  
  corr.factor <- data[,first238]/data[,second238]
  
  data[,second] <- data[,second]*corr.factor
  
  return(data)
}