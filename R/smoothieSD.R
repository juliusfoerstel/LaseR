#' Smooth Curve
#' 
#'  @param x Input data
#'  @param stage At what stage chould the data be smoothed? "X","BG","Mean","corr", tbc
#'  @param  l Length of the gaussian to smooth with, corresponds to the Level of smoothness
#'    @param    measurements Vector with measurement numbers that should be smoothed.
#'     @param   clear.outliers Boolean to indicate whether outliers should be cleared before smoothing.
#' @keywords outlier, sigma, range
#' @export
#' @examples
#' clear.outliers(x = data)
#' clear.outliers(x = data, a = 2, use.quartile = T)



smooth.signal.SD <- function(data, stage = "mbc", l = 6, measurements = c(), clear.outliers = FALSE){
  
  
  n <- max(data$identifier, na.rm = TRUE)
  col <- colnames(data)
  col <- col[grepl(pattern = stage,x = col)]
  coln <- paste("smoothSD", substring(col,nchar(stage)+1),sep = "")
  id <- data$identifier
  
  data[coln] <- NA
  
  if(!length(measurements)){
    these.measurements <- 1:n
  }else{
    these.measurements <- measurements
  }
  
  for(m in these.measurements){
    for(c in 1:length(coln)){
      c.old <- col[c]
      c.new <- coln[c]
      
      is.m <- id == m & !is.na(id) 
      
      is.result <- (range(data$Cycle[is.m])[1]+l/2) : (range(data$Cycle[is.m])[2]-l/2)
      
      is.result <- c(is.result[1]-1,is.result)
      
      if(clear.outliers){
        clear.signal <- clear.outliers(data[is.m,c.old], fill.gaps = TRUE)
      }else{
        clear.signal <- data[is.m,c.old]
      }
      
      for(i in is.result){
          data[i, c.new] <- sd(clear.signal[(2+i-min(data$Cycle[is.m])-l/2):(2+i-min(data$Cycle[is.m])+l/2)] , na.rm = TRUE)
      }
      
    }
  }
  return(data)
}