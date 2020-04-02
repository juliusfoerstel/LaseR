#' add age uncertainty column
#' 
#' @param data Input data
#' @param n.sample Size of sampling for random numbers
#' @keywords outlier, sigma, range
#' @export
#' @examples
#' add.age.col(data, stage = "mbc)





add.age.sd.col <- function(data, n.sample = 1e3){
  data$Age.SD <- NA
  
  #number of measurements
  m <- max(data$identifier, na.rm = T)
  
  for(i in 1:m){#for each measurement
    is.i <- data$identifier == i & !is.na(data$identifier)
    ar28 <- rnorm(n = n.sample,mean = {data$Mean.ar28[is.i]}[1],sd = if(is.na({data$SD.ar28[is.i]}[1])){abs({data$Mean.ar28[is.i]}[1]*0.01)}else{{data$SD.ar28[is.i]}[1]})
    ar28[ar28<0] <- 0
    ar48 <- rnorm(n = n.sample,mean = {data$Mean.ar48[is.i]}[1],sd = if(is.na({data$SD.ar48[is.i]}[1])){abs({data$Mean.ar48[is.i]}[1]*0.01)}else{{data$SD.ar48[is.i]}[1]})
    ar48[ar48<0] <- 0
    ar08 <- rnorm(n = n.sample,mean = {data$Mean.ar08[is.i]}[1],sd = if(is.na({data$SD.ar08[is.i]}[1])){abs({data$Mean.ar08[is.i]}[1]*0.01)}else{{data$SD.ar08[is.i]}[1]})
    ar08[ar08<0] <- 0
    initial30.32 <- runif(n = n.sample,min=0.1,max = 26)
    
    

    ages <- c()
    for(a in 1:n.sample){
      ages[a] <- golden.age.search(ar08[a], ar28[a], ar48[a],
                                   young.limit = 1,old.limit = 6e5, tolerance = 100,initial30.32[a])
    }
    data$Age.SD[is.i] <- sd(ages)
  }
  
  return(data)
}