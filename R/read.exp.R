#' Function to read exp-files
#'
#' This function allows you to load data from exp-files and put them into a data.frame.
#' @keywords read
#' @export
#' @examples
#' read.exp()


read.exp <- function(path = NA){
  if(is.na(path)){
  if(length(list.files(pattern="*.csv",path = 'data/'))>0){
    cat("There already exists at least one csv-file of data in here. To load please use the load.csv function.\n These are the files that already exist:",
        list.files(pattern="*.csv",path = 'data/'),sep = "\n")
  }
  
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
  } else {
    file <- path
  }
  
  
  
  
  lines <- readLines(file) #read the file line by line
  
  
  start.reading <- seq(1,length(lines),by = 1)[grepl(pattern = "Cycle",x = lines)] #find starting line
  options(warn=-1) #turn warnings off
  stop.reading <- tail(seq(start.reading,length(lines),by = 1)[!is.na(as.integer(substring(lines[start.reading:length(lines)],1,1)))],n=1) #find end line
  options(warn=0) #turn warnings back on

  
  #There might be empty lines, that cause a problem
  if(max(grepl(pattern = '\t\t\t',x = lines[start.reading:stop.reading]))){
    for(line in start.reading:stop.reading){
      if(grepl(pattern = '\t\t\t',x = lines[line])){
        stop.reading <- line
        break
      }
    }
  } #look for empty lines
  
  
  cycles <- stop.reading-start.reading -1 #calculate the number of cycles measured
  
  data <- read.table(file,skip = start.reading-1 ,header = T, nrows = cycles-1) #read only the data that is relevant
  
  return(data)
}