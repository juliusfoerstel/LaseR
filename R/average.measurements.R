#' Average measurements
#'
#' This function averages the measurements and adds new columns with the new data and the standard deviation. 
#' @param data Data input
#' @keywords mean, average, sd
#' @export
#' @examples
#' average.measurements(data)

average.measurements <- function(data, stage = "BG", clear.outliers = TRUE){
  
  col <- colnames(data)
  col <- col[grepl(pattern = stage ,x = col)]
  coln <- paste("Mean", substring(col,nchar(stage)+1),sep = "")
  data[coln] <- NA
  colsd <- paste("SD", substring(col,nchar(stage)+1),sep = "")
  data[colsd] <- NA
  
  n <- max(data$identifier, na.rm = TRUE)
  id <- data$identifier
  
  for(measurement in 0:n){
    this.data <- (id==measurement & !is.na(id))
    for(this.col in coln){
      data[this.data, this.col] <- if(clear.outliers){
        mean(clear.outliers(data[this.data,col[this.col==coln]]), na.rm = TRUE) #average
      }else{
        mean(data[this.data,col[this.col==coln]], na.rm = TRUE)
      }
        
      
      data[this.data, colsd[this.col==coln]] <- if(clear.outliers){
        d <- clear.outliers(data[this.data,col[this.col==coln]])
        length <- length(d) - sum(is.na(d))
        sd(d, na.rm = TRUE)/sqrt(length) #Standard error
      }else{
        d <- data[this.data,col[this.col==coln]]
        length <- length(d) - sum(is.na(d))
        sd(d, na.rm = TRUE)/sqrt(length) #Standard error
      }
        
    }
  }
  
  return(data)
}