#' Read iCAP data files
#'
#' This function loads all iCAP data files and returns one data frame, including this data. 
#' @param pattern The pattern in the file names that connects all measurements you want to read. Default ".csv"
#' @param files The files that you want to read. Default NA
#' @keywords read, icap, data, csv-files
#' @export
#' @examples
#' read.iCAP(pattern = "date1_measurementB")



read.iCAP <- function(pattern = ".csv", files = NA){
  
  if(is.na(files[1])){
    files <- list.files(path = ".", pattern = pattern, full.names = F)
  }
  l <- length(files)
  if(l >= 10){
    numbers <- c()
    matches <- regmatches(files, gregexpr("[[:digit:]]+", files))
    for(i in 1:l){
      numbers <- c(numbers, as.numeric({matches[[i]]}[length(matches[[i]])]))
    }
    order <- c()
    for(i in 1:l){
      order[i] <- {1:l}[i==numbers]
    }
    files <- files[order]
  }
  #read all data files and extract metadata
  md <- get_MetaData(files = files)
  
  
  
  data <- read.csv(files[1], header = T, skip = 13, stringsAsFactors = FALSE)
  isotopes <- names(data)
  data$m <- 0
  data <- data[0,]
  
  for(i in 1:length(files)){
    file <- files[i]
    raw.data <- read.csv(file, header = F, skip = 15, stringsAsFactors = FALSE)
    colnames(raw.data) <- isotopes
    raw.data <- raw.data[2:{dim(raw.data)[1]},]
    raw.data$m <- i 
    data <- rbind(data,raw.data)
  }
  data$Index <- 1:dim(data)[1] # add a continuous index column
  data$smpl <- md$name[data$m]
  data <- data[,names(data)!="X"] #delete the empty X column
  return(data)
}
