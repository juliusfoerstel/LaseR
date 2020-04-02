#' Opens the tikzDevice in the graphics folder of the MA
#'
#' This function opens and loads the tikzDevice to create a tikz output for a plot. It uses therein the function create.graphics.path()
#' also from the LaseR-package.
#' @param filename A string which is going to be the name of the output file. Default value is "RPlot"
#'   @param     width numeric Width of the plot in inches. Default is 4.
#'   @param     height numeric height the plot in inches. Default is 3. 
#' @keywords graphics, device, latex file
#' @export
#' @examples
#' plot.for.ma(filename = "Signalstärke", width = 4, height = 4)
#' 
#' This creates a new file called Signal_intensities.tex in the folder where the graphics for the Tex-file are stored.
#' Afterwards the plotting has to be done and in the end the device has to be closed by dev.off().


plot.for.ma <- function(filename = "RPlot", width = 4, height = 3, ... ){
  library("tikzDevice", lib.loc="~/R/win-library/3.3")
  tikz(file = create.graphics.path(filename = filename, replace = TRUE),
       engine = 'luatex', width = width, height = height,  ...)
}
