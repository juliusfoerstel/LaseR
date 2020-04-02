#' Function to write data in a csv file
#'
#' This function allows you to load data from csv-files and put them into a data.frame.
#' @param filename Filename of the csv file to write ".csv". If not specified you are asked to enter a filename.
#' 
#' @keywords write, csv
#' @export
#' @examples
#' save.data()

save.data <- function(data, filename = NA){
  if(is.na(filename)){
    filename <- readline(prompt="Enter file name: ")
  }
  filename <- paste("data/",filename,".csv", sep = "")
  write.csv(x = data, file = filename,na = "NA", row.names = F)
}