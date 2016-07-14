#' Golden Age Search
#'
#' This function calculates the age from the input data.
#' @param data Data frame as input
#' young.limit Lower boundary of age in years. Default is 10.
#' old.limit Upper boundary of age in years. Default is 500,000.
#' tolerance Stop iteration, if boundaries are less tolerance from each other. Default is 1.
#' initial30.32 Value for initial 230Th/232Th value. Default is 5.
#' @keywords golden search, age, decay equation
#' @export
#' @examples
#' golden.age.search(data,tolerance=10)




# decay.equation <- function(t){
#   initial30.32=20
#   return(abs(mean(data$ar08,na.rm = T)
#              -(mean(data$ar28,na.rm = T)*initial30.32*exp(-lambda230*t))
#              -(1-exp(-lambda230*t)+
#                  (mean(data$ar48,na.rm = T)-1)*(lambda230/(lambda230-lambda234))
#                *(1-exp((lambda234-lambda230) *t)))))
# }


golden.age.search <- function(data,
                              young.limit = 10, 
                              old.limit = 5e5,
                              tolerance = 1,
                              initial30.32 = 5){
  golden.ratio = 2/(sqrt(5) + 1)
  
  f <- function(t){ #decay equation
    return(abs(mean(data$ar08,na.rm = T)
               -(mean(data$ar28,na.rm = T)*initial30.32*exp(-lambda230*t))
               -(1-exp(-lambda230*t)+
                   (mean(data$ar48,na.rm = T)-1)*(lambda230/(lambda230-lambda234))
                 *(1-exp((lambda234-lambda230) *t)))))
  }
  #calculate the first test points
  x1 = old.limit - golden.ratio*(old.limit - young.limit)
  x2 = young.limit + golden.ratio*(old.limit - young.limit)
  
  ### Evaluate the function at the test points
  f1 = f(x1)
  f2 = f(x2)
  
  iteration = 0
  
  while (abs(old.limit - young.limit) > tolerance & iteration < 1e3){
    iteration = iteration + 1

    
    if (f2 > f1)
      # then the minimum is to the left of x2
      # let x2 be the new upper bound
      # let x1 be the new upper test point
    {
      ### Set the new upper bound
      old.limit = x2

      ### Set the new upper test point
      ### Use the special result of the golden ratio
      x2 = x1

      f2 = f1
      
      ### Set the new lower test point
      x1 = old.limit - golden.ratio*(old.limit - young.limit)

      f1 = f(x1)
    } 
    else 
    {
      # the minimum is to the right of x1
      # let x1 be the new lower bound
      # let x2 be the new lower test point
      
      ### Set the new lower bound
      young.limit = x1

      
      ### Set the new lower test point
      x1 = x2

      f1 = f2
      
      ### Set the new upper test point
      x2 = young.limit + golden.ratio*(old.limit - young.limit)
      f2 = f(x2)
    }
  }
  
  ### Use the mid-point of the final interval as the estimate of the optimzer

  estimated.minimizer = (young.limit + old.limit)/2
  return(estimated.minimizer)
}


