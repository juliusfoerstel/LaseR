#' Subtract background
#'
#' This function subtracts the mean background off of each measured value.
#' @param data Data input
#' @keywords background
#' @export
#' @examples
#' subtract.background(data)

subtract.background <- function(data){
  if(is.null(data$measurements)){  #if data does not have the col measurement in it
    data <- find.measurements(data) #add it with the function find.measurements
  }
  
  bg <- data[!data$measurements,]
  l <- length(bg)
  
  bg <- lapply(bg[3:(l-1)], clear.outliers)
  bg <- lapply(bg, mean, na.rm = T)
  
  for(index in 3:(l-1)){
    data[as.logical(data$measurements),index] <- data[as.logical(data$measurements),index] - bg[[index-2]]
  }
  return(data)
}




