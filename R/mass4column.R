#' Clear Outliers
#'
#' This function appoints masses to each column and returns a seperate vector.
#' @param data Data input
#' @keywords mass
#' @export
#' @examples
#' mass4column(data)

mass4column <- function(data){
  colnam <- colnames(data)
  masses <- as.data.frame(matrix(data=0,nrow = 1,ncol=length(colnam)))
  colnames(masses) <- colnam
  
  masses[grepl('230',colnam)] <- m30
  masses[grepl('232',colnam)] <- m32
  masses[grepl('234',colnam)] <- m34
  masses[grepl('235',colnam)] <- m35
  masses[grepl('238',colnam)] <- m38
  
  return(masses)
}