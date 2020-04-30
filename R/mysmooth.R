#' my own smooth function
#'
#' This function smooths a signal with a floating mean or a gaussian convolution.
#' @param data vector of data
#' @param l number of points to be averaged
#' @param clear.outliers Boolean
#' @param gaussian Boolean
#' @param pad boolean whether NA should be added in front and in the back to maintain the same length
#' @keywords smooth
#' @export
#'

mySmooth <- function (data, l = 6 , clear.outliers = FALSE, gaussian = TRUE, pad = FALSE){
    x <- seq(0, 1, length.out = l)
    if (gaussian) {
      gauss <- dnorm(x = x, mean = 0.5, sd = 0.2)
      gauss <- gauss/sum(gauss)
    } else {
      gauss <- dunif(x = x, min = 0, max = 1)
      gauss <- gauss/sum(gauss)
    }
    data <- as.data.frame(data)
    if (clear.outliers) {
      clear.signal <- apply(FUN = clear.outliers, X = data, MARGIN = 2, fill.gaps = TRUE, a = 10)
    }
    else {
      clear.signal <- data
    }
    data <- as.data.frame(apply(FUN = convolve,X = clear.signal, MARGIN = 2, y = gauss, type = "filter"))
    if(pad){
      out <- c(rep(NA,(l-1)/2),data$data,rep(NA,(l-1)/2))
    } else {
      out <- data$data
    }
  return(out)
}

