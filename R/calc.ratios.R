#' Calculate ratios
#'
#' This function calculates ratios based on the column, that it receives as input.
#' @param d Vector of data.
#' @param info data.frame with the columns ID with autamic column names and 
#'             Rel.abund with the relative abundance of the isotope 
#' @keywords concentration, relative abundance
#' @export
#' @examples
#' calc.conc(d = data, info = isotopeInfo)


calc.ratios <- function(d, denominator){
  for(col in names(d)){
    if(substring(col,1,1) =="X"){
      d[col] <- d[col]/denominator
    }
  }
  return(d)
}