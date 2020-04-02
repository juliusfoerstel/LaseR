#' Adds y error bars to plot
#'
#' says it all
#' @param x List of x-Values
#' @param        y List of y-values
#'  @param       xsd List of Errors of y
#'   @param      color Color of the error bars
#'   @param      cap Length of the caps in respect to width of plot window, Default is 0.01
#' @keywords errbar, y
#' @export
#' @examples
#' yerrbar(x,y,ysd,color="red", cap =0.01)


yerrbar <- function(x, y, ysd, color, cap = 0.01,...){
  segments(x , y-ysd,x, y+ysd, col = color, ...)
  if(cap != 0){
    e = cap * (par("usr")[4]-par("usr")[3])
    segments(x-e , y-ysd,x+e, y-ysd, col = color, ...)
    segments(x-e , y+ysd,x+e, y+ysd, col = color, ...)
  }
}