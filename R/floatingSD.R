#' my own floating SD
#'
#' This function gives the SD to a floating mean
#' @param data vector of data
#' @param l number of points to be averaged
#' @param clear.outliers Boolean
#' @param pad boolean whether NA should be added in front and in the back to maintain the same length
#' @param iqr boolean whether to use the interquartile range
#' @keywords sd
#' @keywords floating
#' @export
#'

floatingSD <- function (data, l = 6 , clear.outliers = FALSE, pad = FALSE, iqr = F){
  out <- c()
  len <- length(data)
  if (clear.outliers) {
    clear.signal <-clear.outliers(data,fill.gaps = TRUE, a = 10)
  }
  else {
    clear.signal <- data
  }
  for(i in 1:len){
    if(i>l/2 && i <= len-floor(l/2) && !iqr){
      out <- c(out,sd(clear.signal[(i-floor(l/2)):(i+ceiling(l/2-1))], na.rm = T))
    } else if(i>l/2 && i <= len-floor(l/2) && iqr){
      out <- c(out,IQR(clear.signal[(i-floor(l/2)):(i+ceiling(l/2-1))], na.rm = T))
    }
  }
  if(pad){
    out <- c(rep(NA,(l-1)/2),out,rep(NA,(l-1)/2))
  }
  return(out)
}

