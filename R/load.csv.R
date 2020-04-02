#' Function to load csv files
#'
#' This function allows you to load data from csv-files and put them into a data.frame.
#' @param filename Filename of the csv file to load without ".csv". If not specified you are asked to choose an option, when there is more than one file
#' 
#' @keywords read, csv
#' @export
#' @examples
#' load.csv()


load.csv <- function(filename=NA){
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  
  if(is.na(filename)){
    file <- list.files(pattern="*.csv",path = 'data/') #get file names
    num.files <- length(file) #see how many files there are
    if(num.files>1){#if there is more than one file, ask which one should be used
      print(file)
      choose <- as.numeric(readline(prompt = paste('Which of the files above do you want to choose? ',
                                                   ' Please indicate with a number 1 to ', 
                                                   num.files,': ',sep='')))
      file <- file[choose] #select this file
    }
    file <- paste('data/', file, sep="") #correct the path of the file 
  }else{
    if(substrRight(filename,4)==".csv"){
      file <- filename
    }else{
      file <- paste('data/', filename,".csv", sep="")     
    }
  }

  data <- read.csv(file = file)
  
  return(data)
}