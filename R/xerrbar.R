#' Adds x error bars to plot
#'
#' says it all
#' @param x List of x-Values
#'        y List of y-values
#'        xsd List of Errors of X
#'        color Color of the error bars
#'        cap Length of the caps in respect to width of plot window, Default is 0.01
#' @keywords errbar, x
#' @export
#' @examples
#' xerrbar(x,y,xsd,color="red", cap =0.01)


xerrbar <- function(x, y, xsd, color, cap = 0.01,...){
  segments(x-xsd , y,x+xsd, y, col = color, ...)
  e = cap* (par("usr")[4]-par("usr")[3])
  if(cap != 0){
    segments(x-xsd , y-e,x-xsd, y+e, col = color, ...)
    segments(x+xsd , y-e,x+xsd, y+e, col = color, ...)
  }
}