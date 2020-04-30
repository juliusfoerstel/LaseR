#' Average measurements
#'
#' This function averages the measurements and returns a list of average values and standard deviations. The
#' @param data Data input
#' @keywords mean, average, sd
#' @export
#' @examples
#' loaded_data_2_list(data = read.csv("results.csv"))

loaded_data_2_list<- function(data){
  d <- list() 
  for(i in 1:max(data$m,na.rm = T)){
    d[[i]] <- data[data$m == i & data$ms,] #choose all measured data of Sample i (preablation and main ablation)
    if(max(diff(d[[i]]$Index)) > 4){ #check if there is a gap in the index numbers -> time between ablation and preablation
      index <- d[[i]]$Index[max(diff(d[[i]]$Index)) == diff(d[[i]]$Index)] + 2 #index after the last preablation measurement
      d[[i]]<- d[[i]][d[[i]]$Index > index,] #select only data of main ablation i.e. after preablation
    }
  }
  return(d)
}
