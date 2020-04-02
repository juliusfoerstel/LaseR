#' compare to standard
#'
#' This function calculates the elemental fractionation. 
#' @param d Data input
#' @param stdData Data of the composition of the standard materials
#' @param denColumn the name of the column, that is used as the denominator
#' @param sampleList the list of the standards, that were used
#' @keywords mean, average, sd
#' @export
#' @examples
#' average(data)

compare2standard<- function(d, stdData, denColumn, sampleList){
  out <- d
  signal.cols <- substring(names(data),1,1) =="X"
  for(i in 1:length(sampleList)){
    for(columnName in names(d)[signal.cols]){
      mass_1 <- stdData[columnName == stdData$ID & !is.na(stdData$ID) , "mass"]
      mass_d <- stdData[denColumn == stdData$ID & !is.na(stdData$ID) , "mass"]
      v_1 <- stdData[columnName == stdData$ID & !is.na(stdData$ID), {sampleList[i]}]
      v_d <- stdData[denColumn == stdData$ID & !is.na(stdData$ID) , {sampleList[i]}]
      stdValue <- (v_1 / mass_1) / (v_d / mass_d)
      
      if(length(stdValue) == 0){
        stdValue <- NA
      }
      out[i,columnName] <- d[i,columnName] / stdValue
    } 
  }
  return(out)
}