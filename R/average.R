#' Average measurements
#'
#' This function averages the measurements and returns a list of average values and standard deviations. 
#' @param data Data input
#' @keywords mean, average, sd
#' @export
#' @examples
#' average(data)

average<- function(d, clear.outliers = TRUE){
  meanVals <- d[0,]
  sdVals <- d[0,]
  
  n <- max(d$m)

  for(i in 1:n){
    meanVals[i,] <- apply(X = d[d$m==i,], MARGIN = 2, mean, na.rm = TRUE)
    sdVals[i,] <- apply(X = d[d$m==i,], MARGIN = 2, sd, na.rm = TRUE)
  }
  
  return(list(meanVals,sdVals))
}