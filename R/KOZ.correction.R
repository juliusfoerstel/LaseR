#' KOZ correction
#'
#' This function corrects the actvity ratios by taking the standard measurement of KOZ into account
#' @param data Data frame with original data.
#'  standard.measurements A list of numbers which were identified to be standards.
#'  stage The ratios are supposed to be build on what stage? "X","BG","Mean","corr", tbc
#' @keywords ratios
#' @export
#' @examples
#' KOZ.correct(data)


KOZ.correct <- function(data, standard.measurements = c(1,3), stage = "mbc"){
  n <- length(standard.measurements)-1 #number of how often you can fit a linear model
  ratio.cols <- paste(stage,".", c("ar08","ar48","ar28"),sep="")
  coln <- paste("STDc", substring(ratio.cols,nchar(stage)+1),sep = "")
  id <- data$identifier
  
  
  for(i in 1:n){
    is.std <- (id==standard.measurements[i] | id==standard.measurements[i+1]) & !is.na(id)
    std <- apply(FUN = mean, X = data[is.std,ratio.cols], MARGIN = 2, na.rm = TRUE)

    corr08 <- koz.ar08/std[1]
    corr48 <- koz.ar48/std[2]
    corr28 <- koz.ar28/std[3]
    new <- data[ratio.cols]
    colnames(new) <- coln
    new[,1] <- data[ratio.cols[1]]*corr08 
    new[,2] <- data[ratio.cols[2]]*corr48
    new[,3] <- data[ratio.cols[3]]*corr28
    
   
    data[coln] <- new
    
  }
  
  return(data)
  
}