





get_MetaData <- function(files){
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
  
  name <- c()
  for(i in 1:length(files)){
    lines <- read.csv(file = files[i], header = F, nrows = 14, stringsAsFactors = F)
    name[i] <- substr(x = lines[1,],start = 1,stop = regexpr(pattern = ":", text = lines[1,])[1] - 1 )
    
    
  }
  
  return(data.frame(name,stringsAsFactors = F))
  
  
}



