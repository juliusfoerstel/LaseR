#' Add Ratios
#'
#' This function adds measured and activity ratios to your data frame.
#' @param data Data frame with original data.
#'  stage The ratios are supposed to be build on what stage? "X","BG","Mean",tbc
#'  ox Boolean to say whether this is an oxid-measurement.
#' @keywords ratios
#' @export
#' @examples
#' add.ratios(data)

add.ratios <- function(data, stage = "BG", ox = FALSE){
  if(ox){
    colnames.blank <- c("ThO", "UO", "UH", "ThU" )
    new.colnames <- paste(stage,".", colnames.blank, sep = "")
    meas.names.blank <- c("1.232Th","1.238U","1.248","1.254", "2.238U", "2.239")
    new.meas.names <-   paste(stage,meas.names.blank, sep = "")
    
    data[new.colnames[1]] <- data[new.meas.names[3]] / data[new.meas.names[1]]
    data[new.colnames[2]] <- data[new.meas.names[4]] / data[new.meas.names[2]]
    
    data[new.colnames[3]] <- data[new.meas.names[6]] / data[new.meas.names[5]]
    data[new.colnames[4]] <- data[new.meas.names[1]] / data[new.meas.names[2]]
    
    
  }else{
    colnames.blank <- c("mr08","ar08","mr48","ar48","mr02","ar02","mr28","ar28", "mr85" )
    new.colnames <- paste(stage,".", colnames.blank, sep = "")
    meas.names.blank <- c("1.230Th","1.238U","2.232Th","2.234U", "2.235U", "2.238U")
    new.meas.names <-   paste(stage,meas.names.blank, sep = "")
    
    
    data[new.colnames[1]] <- data[new.meas.names[1]] / data[new.meas.names[2]] / res11
    data[new.colnames[2]] <- data[new.colnames[1]] *con08
    
    data[new.colnames[3]] <- data[new.meas.names[4]]/data[new.meas.names[6]] / res11
    data[new.colnames[4]] <- data[new.colnames[3]] *con48
    
    data[new.colnames[5]] <- data[new.meas.names[1]]/data[new.meas.names[3]] / res11
    data[new.colnames[6]] <- data[new.colnames[5]] *con02
    
    data[new.colnames[7]] <- data[new.meas.names[3]]/data[new.meas.names[6]]
    data[new.colnames[8]] <- data[new.colnames[7]] *con28
    
    data[new.colnames[9]] <- data[new.meas.names[6]]/data[new.meas.names[5]]
  }

  return(data)
}