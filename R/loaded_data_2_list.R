#' Average measurements
#'
#' This function averages the measurements and returns a list of average values and standard deviations. 
#' @param data Data input
#' @param preablation Boolean if True preablation is discarded
#' @keywords mean, average, sd
#' @export
#' @examples
#' loaded_data_2_list(data = read.csv("results.csv"), preablation = T)

loaded_data_2_list<- function(data, preablation = T){
  d <- list() 
  for(i in 1:max(data$m,na.rm = T)){
    d[[i]] <- data[data$m == i & data$ms,] #choose all measured data of Sample i (preablation and main ablation)
    index <- d[[i]]$Index[max(diff(d[[i]]$Index)) == diff(d[[i]]$Index)] + 2 #index after the last preablation measurement
    d[[i]]<- d[[i]][d[[i]]$Index > index,] #select only data of main ablation i.e. after preablation
  }
  return(d)
}
