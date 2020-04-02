#' Clean Edges
#'
#' This function deletes the first and last entry of each measurement.
#' @param data Input data
#' @keywords uptake, memory, clean edges
#' @export
#' @examples
#' clean.edges(data)

clean.edges <- function(data){
  data <- data[as.logical(data$measurements),] # only use data without background
  n <- max(data$measurements) #number of measurements
  
  drop <- c() #List of the cycles to drop
  for(index in 1:n){
    drop <- c(drop,head(data[data$measurements==index,"Cycle"],n = 1))
    drop <- c(drop,tail(data[data$measurements==index,"Cycle"],n = 1))
  }
  
  return(data[!is.element(data$Cycle,drop),])
}