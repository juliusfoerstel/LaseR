plot.ratios <- function(from = 0, to=NA){
  data <- read.exp()
  cups <- get.cups()
  data <- convert2counts(data = data,cups = cups)
  data <- find.measurements(data,threshold = 40)
  #data <- data[!(!data$measurements),]
  if(is.na(to)){
    to <-  dim(data)[1]
  }
  data <- data[from:to,]
  pdf("plots/ratios.pdf", paper='a4')
  
  
  
  plot(data$X1.230Th/data$X1.238U*con08, type="l", ylim= c(0,1.5))
  grid()
  plot(data$X2.232Th/data$X2.238U, type="l")
  grid()
  plot(data$X2.234U/data$X2.238U*con48,type="l", ylim= c(0,1.5))
  grid()
  plot(data$X2.238U/data$X2.235U, type="l",ylim = c(0,300))
  grid()
  dev.off()
}