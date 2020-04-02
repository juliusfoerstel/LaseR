#' Create a new folder for a measurement
#'
#' This function creates a new folder with the subfolders "data" and "plots".
#' @keywords folder, measurement
#' @export
#' @examples
#' create.new.measurement()

create.new.measurement.folder <- function(){
  n <- readline(prompt="Enter folder name: ")
  dir.create(path = paste("./",n,sep = ""))
  dir.create(path = paste("./",n,"/plots",sep = ""))
  dir.create(path = paste("./",n,"/data",sep = ""))
}


