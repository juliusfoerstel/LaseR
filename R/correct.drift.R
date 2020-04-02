#' correct drift
#'
#' This function drift, by taking into account how a standard was repeatedly measured.
#' @param data Data frame with original data.
#'  standard.measurements A list of numbers which were identified to be standards.
#'  stage The ratios are supposed to be build on what stage? "X","BG","Mean","corr", tbc
#' @keywords ratios
#' @export
#' @examples
#' add.ratios(data)


correct.drift <- function(data, standard.measurements = c(1,3), stage = "BG"){
  n <- length(standard.measurements)-1 #number of how often you can fit a linear model
  col <- colnames(data)
  col <- col[grepl(pattern = stage,x = col)]
  coln <- paste("corr", substring(col,nchar(stage)+1),sep = "")
  id <- data$identifier
  
  for(i in 1:n){
    is.start <- id==standard.measurements[i] & !is.na(id)
    is.end  <- id==standard.measurements[i+1] & !is.na(id)
    start <- apply(FUN = mean, X = data[is.start,], MARGIN = 2, na.rm = TRUE)
    end <- apply(FUN = mean, X = data[is.end,], MARGIN = 2, na.rm = TRUE)
    diff <-  start/end - 1
    diff.per.time <- diff/(end["Time"]-start["Time"])
    
    t.start <- start["Time"]
    
    
    data[coln] <- data[col] / ( 1 - (data$Time-t.start) %*% t(diff.per.time[col]))
  }
  
  return(data)
  
}