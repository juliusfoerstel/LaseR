#' Subtract background
#'
#' This function subtracts the mean background off of each measured value and adds new columns with the new data. 
#' @param data Data input
#' @keywords background
#' @export
#' @examples
#' subtract.background(data)

subtract.background <- function(data){
  bg.only <- apply(FUN = clear.outliers,X= data[data$identifier==0,],MARGIN = 2, a = 1.5)
  mean.bg <- apply(FUN = mean, X = bg.only ,MARGIN = 2, na.rm = TRUE)
  

  coln <- colnames(data)
  coln <- coln[grepl(pattern = "X",x = coln)]
  
  for(col in coln){
     data[paste("BG", substring(col,2),sep = "")] <- data[col] - mean.bg[col]
  }
  
  return(data)
}




