#' Import information from Laser Experiment
#'
#' This function reads the csv file of a laser experiment
#' @keywords laser info
#' @export
#' @examples
#' import.LaserExperiment()


import.LaserExperiment <- function(file = NA){
  if(is.na(file)){
    if(grepl(pattern = "Linux", x = Sys.info()[1])){
      library(tcltk)
      file <- tk_choose.files(multi = F) #choose the experiment file. the prompt is not nice, but it works on all systems (I hope)
    } else {
      file <- choose.files()
    }
  }
  lasdat <- read.csv(file = file, stringsAsFactors = F)
  is.refPoint <- lasdat$Pattern.Type == 7
  is.track <- lasdat$Pattern.Type == 4
  start <- {1:length(names(lasdat))}[names(lasdat) == "Ablation.Pass"]
  this.cols <- c(4,1,start+c(4,7,8,9,11,15,18:23))
  out <- lasdat[,this.cols]
  tracks <- {1:length(is.track)}[is.track & !is.na(is.track)]
  out$start.x <- NA
  out$start.y <- NA
  out$start.z <- NA
  out$end.x <- NA
  out$end.y <- NA
  out$end.z <- NA
  out$track.length
  initial <- c("X", "Y", "Z")
  s <- c("start.x","start.y", "start.z")
  e <- c("end.x","end.y", "end.z")
  for(track.line in tracks){
    out[track.line,s] <-  out[track.line,initial]
    out[track.line,e] <-  out[track.line+1,initial]
    out[track.line,"track.length"] <- sqrt(sum(({out[track.line,s]-out[track.line,e]})^2))
  }
  out$is.refPoint <- is.refPoint
  out$is.track <- is.track
  out <- out[!is.na(is.track),]
  
  return(out)
  
}