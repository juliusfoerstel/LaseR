plot.raw.data <- function(from = 0, to=NA){
  data <- read.exp()

  cups <- get.cups()
  data <- convert2counts(data = data,cups = cups)
  data <- find.measurements(data,threshold = 20)
  
  #data <- data[!(!data$measurements),]
  data <- as.data.frame(lapply(data,clear.outliers,a=5))
  
  if(is.na(to)){
    to <-  dim(data)[1]
  }
  data <- data[from:to,]
  pdf("plots/signals.pdf", paper='a4')
    
  plot(data$X1.230Th, type="l")
  grid()
  plot(data$X1.238U, type="l")
  grid()
  plot(data$X2.232Th, type="l")
  grid()
  plot(data$X2.234U,type="l")
  grid()
  plot(data$X2.235U, type="l")
  grid()
  plot(data$X2.238U, type="l")
  grid()
  dev.off()
}