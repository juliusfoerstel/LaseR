#' Fast evaluation
#'
#' This function uses all other functions to calculate an age.
#' @keywords combination
#' @export
#' @examples
#' fast.eval()

fast.eval <- function(){
  data <- read.exp()
  cups <- get.cups()
  data <- convert2counts(data = data,cups = cups)
  data <- find.measurements(data,threshold = 20)
  write.table(x=data,file = 'data/auswertung.csv',sep=';',dec='.',col.names = colnames(data))
  
  data <- subtract.background(data)
  #data <- as.data.frame(lapply(data,clear.outliers))
  data <- intracycle.correction(data)
  #data <- clean.edges(data)
  data <- mb58.correction(data)
  #data <- as.data.frame(lapply(data,clear.outliers))
  data <- add.ratios(data)
  write.table(x=data,file = 'data/auswertung2.csv',sep=';',dec='.',col.names = colnames(data))
  
  #data <- as.data.frame(lapply(data,clear.outliers))
  out <- average.measurements(data)
  
  
  age <- 1:dim(out)[1]
  age <- NA
  age.e <- 1:dim(out)[1]
  age.e <- NA
  for(i in 1:dim(out)[1]){
    if(!is.na(out[i,'ar08'])){
      age[i] <- golden.age.search(out[i,])
      #age.e[i] <- age.error(out[i,])
    }
  }
  #age.e <- age.error(out[1,])
  #cat('age = (',round(age),'+/-',round(age.e),')kyrs')
  #plot(out$ar08,col=data$measurements,pch=20)
  
  
  return(data)
}