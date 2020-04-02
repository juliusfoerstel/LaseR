#' Convert the Time stamps to real dates
#' 
#' returns data frame with an additional column "dates" including the dates
#'
#' @keywords date, seconds
#' @param data Data frame 
#' @export
#' @examples
#' s2date(df = data)


s2date <- function(df){
  df$dates <- as.POSIXct(df$Time, origin = "1970-01-01",tz = "")
  return(df)
}