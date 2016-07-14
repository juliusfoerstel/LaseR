#' Function to read exp-files
#'
#' This function allows you to load data from exp-files and put them into a data.frame.
#' @keywords read
#' @export
#' @examples
#' read.exp()


read.exp <- function(){
  
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
  
  start.reading <- seq(1,length(lines),by = 1)[grepl(pattern = "Cycle",x = lines)] #find starting line
  stop.reading <- seq(1,length(lines),by = 1)[grepl(pattern = "Cup",x = lines)] #find end line
  
  #There might be empty lines, that cause a problem
  if(max(grepl(pattern = '\t\t\t\t\t\t\t\t',x = lines[start.reading:stop.reading]))){
    for(line in start.reading:stop.reading){
      if(grepl(pattern = '\t\t\t\t\t\t\t\t',x = lines[line])){
        stop.reading <- line
        break
      }
    }
  } #look for empty lines
  
  
  cycles <- stop.reading-start.reading -1 #calculate the number of cycles measured
  
  data <- read.table(file,skip = start.reading-1 ,header = T, nrows = cycles-1) #read only the data that is relevant
  
  return(data)
}