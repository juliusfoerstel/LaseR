#' Update this Package
#'
#' This function updates the LaseR-package
#' @param list list of data frames
#' @param folder name of the folder where the files should be stored
#' @param filename default filename
#' @keywords update,change
#' @export
#' @examples
#' write.multiple.csv(list = data, folder = "new", filename = "results")

write.multiple.csv <- function(list, folder, filename){
  fileTypes <- c("rawData", "bgCorrected", "Conc", "norm", "average", "sd", "frac")
  dir.create(folder)
  setwd(folder)
  for(i in 1:length(list)){
    write.csv(x = list[[i]], file = paste0(filename,"_",i,"_",fileTypes[i],".csv"))
  }
  setwd("..")
}

