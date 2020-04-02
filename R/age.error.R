#' Age Error
#'
#' This function calculates the age errors by using a MC method.
#' @param data Data with errors to calculate the age errors.
#' @keywords age, error
#' @export
#' @examples
#' age.error(data)


age.error <- function(data){
  
  n.sample <- 1e3
  ar28 <- rnorm(n = n.sample,mean = data$ar28,sd = data$ar28.e/2)
  ar28[ar28<0] <- 0
  ar48 <- rnorm(n = n.sample,mean = data$ar48,sd = data$ar48.e/2)
  ar48[ar48<0] <- 0
  ar08 <- rnorm(n = n.sample,mean = data$ar08,sd = data$ar08.e/2)
  ar08[ar08<0] <- 0
  initial30.32 <- runif(n = n.sample,min=0.1,max = 26)
  
  df <- data.frame(ar28,ar48,ar08,initial30.32)
  ages <- c()
  for(i in 1:n.sample){
    ages[i] <- golden.age.search(data = df[i,])
  }
  return(sd(ages)*2)
}
# f <- function(t){ #decay equation
#   initial30.32=5
#   return(abs(mean(data$ar08,na.rm = T)
#              -(mean(data$ar28,na.rm = T)*initial30.32*exp(-lambda230*t))
#              -(1-exp(-lambda230*t)+
#                  (mean(data$ar48,na.rm = T)-1)*(lambda230/(lambda230-lambda234))
#                *(1-exp((lambda234-lambda230) *t)))))
# }
