#' Function evaluate oxide/Hydride measurements
#'
#' This function calculates oxide and hydride ratios after substracting the background.
#' @keywords oxhy, evaluate
#' @export
#' @examples
#' oxhy()

oxhy <- function(){
  data <- load.csv()
  id <- data$identifier
  data <- data[,1:9]
  data$identifier <- id
  data <- subtract.background(data)
  data <- add.ratios(data = data, stage = "BG", ox = TRUE)
  data <- average.measurements(data, stage = "BG")
  return(data)
}