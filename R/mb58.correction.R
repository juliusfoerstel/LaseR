#' Mass Bias Correction
#'
#' This function corrects for mass bias based on an exponential law and the natural ratio of 238U/235U.
#' @param data Data input.
#' @keywords mass bias
#' @export
#' @examples
#' mb58.correction(data)

mb58.correction <- function(data, stage = "corr"){
  masses <- mass4column(data)
  col <- colnames(data)
  coln <- col[grepl(pattern = stage,x = col)]
  new.coln <- paste("mbc", substring(coln,nchar(stage)+1),sep = "")
  data.double <- data 
  

  if(sum(grepl(pattern = "mr85", x = col))){
    ratio <- data[,grepl(pattern = paste(stage,".mr85",sep = ""), x = col)]
  }else{
    ratio <- data[,grepl(pattern = paste(stage,"2.238U",sep = ""), x = col)]/
      data[,grepl(pattern = paste(stage,"2.235U",sep = ""), x = col)]
  
  }
  ratio[ratio<=0] <- 1
  mb.factor <- log(ratio/ratio85)/(m38-m35)
  
  for(mass in c(m30,m32,m34,m35,m38)){
      data.double[,masses==mass] <- data[,masses==mass] * exp(mb.factor * (m38-mass))
  }
  
  data[new.coln] <- data.double[coln] 
 
  return(data)
}