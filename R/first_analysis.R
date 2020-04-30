#' Read raw data from the icap
#' @param output.filename string default filename of output
#' @param subtract.bg boolean whether backround is already subtracted, default = T
#' @param dismiss.beginning integer no of measurement points discarded
#' @param dismiss.after integer no of measurement points discarded
#' @param step integer no of measurement points displayed 
#' @param exp.bg boolean approximate background with an axponential curve, default: F
#' @keywords rawdata, read data
#' @export



first.analysis <- function(matrix.element = "Ca", output.filename = "results", subtract.bg = T, dismiss.beginning = 10, dismiss.after = 10,step = 500, exp.bg = F){
  if(grepl(pattern = "Linux", x = Sys.info()[1])){
    library(tcltk)
    files <- tk_choose.files(filters = matrix(c("CSV-Files",".csv"), nrow = 1, ncol = 2)) #choose the files to evaluate. the prompt is not nice, but it works on all systems (I hope)
  } else {
    files <- choose.files(filters = matrix(c("CSV-Files",".csv"), nrow = 1, ncol = 2))
  }
  
  if(length(files) == 0){stop("No files selected")} #error if no file was selected
  
  #extract directory from file paths
  directory <- gsub("\\","/",files[1], fixed = TRUE)
  end <- tail(gregexpr(pattern = "/", text = directory)[[1]],1)
  directory <- substr(x = directory, start = 1, stop = end)
  
  
  #read all data files and merge them into one big data frame
  data <- read.iCAP(files = files) 
  
  column <- {names(data)[grepl(pattern = matrix.element, names(data))]}[1]
  
  #identify measurements and background (click click)
  mubg <- id.meas.and.bg(d = data[,column], dismiss.beginning = dismiss.beginning , dismiss.after = dismiss.after,step = step) 
  #mubg is a data frame with a ms column with boolean values, whether this is a measurement or background
  data <- cbind(data,mubg) #add mubg to the data
  
  
  #this part subtracts the background
  if(subtract.bg){
    bg.data <- data #make a copy of the original data.frame
    signal.cols <- substring(names(data),1,1) =="X" #Signal columns start with an "X"
    for(i in 1:max(data$m)){ # go through all measurements
      #apply the background correction to all columns
      bg.data[data$m == i,signal.cols] <- apply(X = data[data$m == i,signal.cols], MARGIN = 2, FUN = background.correction, 
                                                is.bg = data$bg[data$m == i], exp = exp.bg) 
    }
    all.data <- list(data, bg.data)
  } else {
    all.data <- list(data)
  }
  
  #go to the data directory and save the evaluated data
  setwd(directory)
  write.multiple.csv(list = all.data,folder = "evaluated", filename = output.filename)
}
