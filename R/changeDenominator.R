#' Change denominator
#'
#' This function changes ratios based on the column, that it receives as input.
#' @param d Vector of data.
#' @param new.denominator string of the new denominator name (eg: "23Na" or "X23Na") 
#' @keywords concentration, relative abundance, change, denominator
#' @export
#' @examples
#' changeDenom(d = data, new.denominator = "23Na")


changeDenom <- function(d, new.denominator){
  d.copy <- d
  if(substring(new.denominator,1,1) !="X"){
    new.denominator <- paste0("X",new.denominator)
  }
  
  
  for(i in 1:dim(d)[1]){
    d.copy[i,] <- d[i,]/d[i,new.denominator]
  }

  return(d.copy)
}