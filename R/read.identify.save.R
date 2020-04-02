#' ris (read identify save)
#'
#' This function reads in the exp file, plots the 238U-Signal and lets you identify where the measurements are and what is background. 
#' All this is then saved in a csv file.
#' @param len Length of the plot in x-direction
#' @keywords file, measurements, background, csv, file
#' @export
#' @examples
#' 


ris <- function(len = 100){
  
  data <- read.exp()
  
  d <- data[,grepl(pattern = "238U",colnames(data))]
  if(is.null(dim(d))){}else{
    d <- d[,1]
  }
  
  
  l.d <- length(d)
  data$identifier <- rep(0,length.out = l.d)
  
  
  
  start.plot <- 1
  end.plot <- if(start.plot + len < l.d){start.plot + len}else{l.d}
  range <- start.plot:end.plot
  
  index <- 1
  while(start.plot < l.d){
    plot(data$Cycle[range], d[range], panel.first = grid())
    cat("Please click on beginning of the next (",index,".) measurement!\n")
    start.m <- range[identify(x = range, d[range] ,n = 1)]
    if(length(start.m)==0 && end.plot==l.d){break}else{
      if(length(start.m)==0){
        start.plot <- end.plot
        end.plot <- if(start.plot + len < l.d){start.plot + len}else{l.d}
        range <- start.plot:end.plot
        next
      }
    }
    start.plot <- start.m
    end.plot <- if(start.plot + len < l.d){start.plot + len}else{l.d}
    range <- start.plot:end.plot
    
    end.m <- integer(0)
    while(length(end.m) == 0){
      plot(data$Cycle[range], d[range], panel.first = grid())
      cat("Please click on end of the ",index,". measurement!\n")
      end.m <- range[identify(x = range, d[range] ,n = 1)]
      start.plot <- if(length(end.m)!=0){end.m+1}else{end.plot}
      end.plot <- if(start.plot + len < l.d){start.plot + len}else{l.d}
      range <- start.plot:end.plot
    }

    signal <- (start.m+1):(end.m-1)
    ignore <- c(start.m-1,start.m, end.m, end.m+1)
    data$identifier[signal] <- index
    data$identifier[ignore] <- NA
    
    index <- index+1
  }

  
  filename <- readline(prompt="Enter file name: ")
  filename <- paste("data/",filename,".csv", sep = "")
  write.csv(x = data, file = filename,na = "NA", row.names = F)
}