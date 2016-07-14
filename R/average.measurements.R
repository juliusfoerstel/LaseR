#' Average the measurements
#'
#' This function combines the data of each individual measurement and adds the standard deviation as an additional column.
#' @param data Data input
#' @keywords mean, average
#' @export
#' @examples
#' average.measurements(data)

average.measurements <- function(data){
  n <- max(data$measurements)
  
  out <- as.data.frame(matrix(data=NA,nrow = n, ncol=dim(data)[2]))
  colnames(out) <- colnames(data)
  colnames(out)[1] <- "measurement"
  
  out.e <- as.data.frame(matrix(data=NA,nrow = n, ncol=dim(data)[2]))
  colnames(out.e) <- paste(colnames(data),".e",sep="")
  colnames(out.e)[1] <- "measurement"
  
  
  for(index in 1:n){
    l <- length(data$measurements[data$measurements==index]) #length of data for this measurement
    out[index,] <- as.data.frame(lapply(X = data[data$measurements==index,],mean,na.rm=T))
    out.e[index,] <- as.data.frame(lapply(X = data[data$measurements==index,],sd,na.rm=T))
    out[index,1] <- index
    out.e[index,1] <- index
    out.e[index,-1] <- out.e[index,-1]/sqrt(l)*2
  }
  out.total <- merge(out,out.e,by = "measurement")
  return(out.total)
}