#' Smooth Curve
#' 
#'  @param x Input data
#'        stage At what stage chould the data be smoothed? "X","BG","Mean","corr", tbc
#'        l Length of the gaussian to smooth with, corresponds to the Level of smoothness
#'        measurements Vector with measurement numbers that should be smoothed.
#'        clear.outliers Boolean to indicate whether outliers should be cleared before smoothing.
#' @keywords outlier, sigma, range
#' @export
#' @examples
#' clear.outliers(x = data)
#' clear.outliers(x = data, a = 2, use.quartile = T)



smooth.signal <- function(data, stage = "mbc", l = 6, measurements = c(), clear.outliers = FALSE, gaussian = TRUE){
  
  
  n <- max(data$identifier, na.rm = TRUE)
  col <- colnames(data)
  col <- col[grepl(pattern = stage,x = col)]
  coln <- paste("smooth", substring(col,nchar(stage)+1),sep = "")
  id <- data$identifier
  
  data[coln] <- NA
  
  if(!length(measurements)){
    these.measurements <- 1:n
  }else{
    these.measurements <- measurements
  }
  
  for(m in these.measurements){
    is.m <- id == m & !is.na(id) 
    
    x <- seq(0,1,length.out = l)
    if(gaussian){
      gauss <- dnorm(x = x,mean = 0.5,sd = 0.2)
      gauss <- gauss/sum(gauss)
    }else{
      gauss <- dunif(x = x,min = 0, max = 1)
      gauss <- gauss/sum(gauss) 
    }
    
    
    is.result <- (range(data$Cycle[is.m])[1]+l/2) : (range(data$Cycle[is.m])[2]-l/2)
    
    is.result <- c(is.result[1]-1,is.result)
    
    if(clear.outliers){
      clear.signal <- apply( FUN = clear.outliers, X = data[is.m,col], MARGIN = 2, fill.gaps = TRUE)
    }else{
      clear.signal <- data[is.m,col]
    }
    data[is.result, coln] <- as.data.frame(apply(FUN = convolve, X = clear.signal, MARGIN = 2, y= gauss, type = "filter"))
  }
  return(data)
}