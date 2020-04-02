#' Calculate the "true" concentration 
#'
#' This function calculates the "true" concentration of the element, based on the natural relative abundance.
#' @param d Vector of data.
#' @param info data.frame with the columns ID with autamic column names and 
#'             Rel.abund with the relative abundance of the isotope 
#' @keywords concentration, relative abundance
#' @export
#' @examples
#' calc.conc(d = data, info = isotopeInfo)


calc.conc <- function(d, info){
  for(col in names(d)){
    if(is.element(col, info$ID)){
      d[,col] <- d[,col] / info$Rel.abund[col==info$ID] * 100
    }
  }
  return(d)
}