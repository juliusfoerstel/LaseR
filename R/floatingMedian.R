#' my own floating median
#' 
#' This function gives the floating median with a given number of data points
#' @param data vector of data
#' @param l number of points to be averaged
#' @param clear.outliers Boolean
#' @param pad boolean whether NA should be added in front and in the back to maintain the same length
#' @keywords sd
#' @keywords floating
#' @export
#'


floatingMedian <- function (data, l = 6 , clear.outliers = FALSE, pad = FALSE){
  out <- c()
  len <- length(data)
  
  
  if (clear.outliers) {
    clear.signal <-clear.outliers(data, fill.gaps = F, a = 10)
  }
  else {
    clear.signal <- data
  }
  
  
  for(i in 1:len){
    if(i>l/2 && i <= len-floor(l/2)){
      out <- c(out,median(clear.signal[(i-floor(l/2)):(i+ceiling(l/2-1))], na.rm = T))
    }
  }
  
  
  if(pad){
    out <- c(rep(NA,(l-1)/2),out,rep(NA,(l-1)/2))
  }
  return(out)
}

