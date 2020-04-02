#' Create a path to the desired file where all graphics are stored
#'
#' This function creates a filepath to save a graphic to the folder where all graphics are stored for my masters thesis.
#' The file can be replaced if the parameter 'replace' is set to TRUE. Otherwise this function checks whethter this file already exists
#' and alters the filename if necessary.
#' @param filename A string which is going to be the name the output file. Default value is "RPlot"
#'        replace Boolean to indicate whether to check if the file should be replaced if it already exists. Default value is FALSE. 
#' @keywords graphics, path, latex file
#' @export
#' @examples
#' create.graphics.path(filename = "Signal_intensities", replace = F)
#' 
#' This creates a new file called Signal_intensities.tex in the folder where the graphics for the Tex-file are stored.
#' If there is already a file with this name it is changed to Signal_intensities_1.tex.



create.graphics.path <- function(filename="RPlot",replace = F){

  graphics.path <- "C:/Users/Jennifer/Dropbox/Master/Masterarbeit/graphics" # this is where the files are stored
  full.filename <- paste(filename,".tex",sep="")  # add the file extension to the filename
  
  path <- file.path(graphics.path,full.filename)   #create the desired filepath for the graphics
  
  #In Case this file already exists, it has to be changed if it should not be replaced
  if(!replace){ #do not check when it should be replaced
    x<-1 #this number will be added to the file path
    while(file.exists(path)){ #change the name as long as the file already exists
      new.filename <- paste(filename,"_",x,".tex",sep="") #append number to the end of the filname to make it unique
      path <- file.path(graphics.path, new.filename) #assemble new file path
      
      if(!file.exists(path)){ #if the file path is unique
        cat("The filename you entered already exists. The filename was changed to",new.filename) #show message as to notify user
      }else{
        x <- x+1  #increase the appended number by 1 if the file is not unique already
      }
    }
    
  }
  return(path)
  
}