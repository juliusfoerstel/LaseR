#' Mass Bias Correction
#'
#' This function corrects for mass bias based on an exponential law and the natural ratio of 238U/235U.
#' @param data Data input.
#' @keywords mass bias
#' @export
#' @examples
#' mb58.correction(data)

mb58.correction <- function(data){
  
  masses <- mass4column(data)

  mb.factor <- log((data$X2.238U/data$X2.235U)/ratio85)/(m38-m35)
  
  for(mass in c(m30,m32,m34,m35,m38)){
    if(mass){
      data[,masses==mass] <- data[,masses==mass] * exp(mb.factor * (m38-mass))
    }
  }
  return(data)
}