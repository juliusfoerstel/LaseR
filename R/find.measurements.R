#' Find Measurements
#'
#' This function appoints numbers to each measurement.
#' @param data Data input.
#'     threshold Maximum deviation from the natural ratio 238U/235U.
#' @keywords measurements, find
#' @export
#' @examples
#' find.measurement(data, threshold=5)


find.measurements <- function(data,threshold = 10){
  cycles <-nrow(data)
  
  background <- if(sum(grepl(pattern = "238U",colnames(data)))>1){#version for more than one U238
                    abs(apply(X = data[,grepl(pattern = "238U",colnames(data))],MARGIN = 1,FUN = sum)/2 / data[,grepl(pattern = "235U",colnames(data))] -ratio85) > threshold
                  }else{#version for only one U238
                    abs(data[,grepl(pattern = "238U",colnames(data))]/data[,grepl(pattern = "235U",colnames(data))] -ratio85) > threshold
                  } 
  
  measurement <- rep(x = 0,length.out=cycles)
  measurement.number <- 1
  for(index in 1:cycles){
    if(!background[index]){
      measurement[index] <- measurement.number
    }else{
      if(index>1){
        if(measurement[index-1]){
          measurement.number <- measurement.number+1
        }
      }
    }
  }
  
  data$measurements <- measurement
  
  return(data)
}