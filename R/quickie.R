#' quick evaluation without drift correction or a standard normalization
#' 
#' You have to have the csv file data.csv
#' @keywords quick, 
#' @export
#' @examples
#' add.age.col(data, stage = "mbc)


quick.auswertung <- function(){
  
  data <- load.csv(filename = "data")
  drop <- c("X2.238U.2.235U","X3.232Th")
  data <- data[,!(names(data) %in% drop)]
  data <- data[,1:9]
  #lodata <- outlier.out(data = data)
  
  data <- subtract.background(data)
  
  data <- mb58.correction(data, stage = "BG")
  
  data <- add.ratios(data = data, stage = "mbc")
  
  data <- average.measurements(data, stage = "mbc")
  
  data <- add.age.col(data = data, stage = "Mean")
  data <- add.age.sd.col(data = data, n.sample = 1e3 )
  save.data(data = data, filename = "ausgewertet_comp")
  
}