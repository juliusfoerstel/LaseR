#' Get cups
#'
#' This function reads, what Cup or IC was used to measure each column.
#' @keywords cups
#' @export
#' @examples
#' get.cups()


get.cups <- function(){
  
  file <- list.files(pattern="*.exp",path = 'data/') #get file names
  num.files <- length(file) #see how many files there are
  if(num.files>1){#if there is more than one file, ask which one should be used
    print(file)
    choose <- as.numeric(readline(prompt = paste('Which of the files above do you want to choose? ',
                                                 ' Please indicate with a number 1 to ', 
                                                 num.files,': ',sep='')))
    file <- file[choose] #select this file
  }
  file <- paste('data/', file, sep="") #correct the path of the file 
  
  
  lines <- readLines(file) #read the file line by line
  
  
  read.line <- seq(1,length(lines),by = 1)[grepl(pattern = "Cup",x = lines)] #find line with cups

  data <- read.table(file,skip = read.line-1 ,header = F, nrows = 1) #read only the cup line

  data <- data[3:8]
  
  return(data)
}