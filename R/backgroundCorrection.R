#' Function to correct the background
#'
#' This function subtracts the background based on a linear or exponential fit.
#' @param d Vector of data.
#' @param is.bg Vector of same length as d with boolean entries.
#' @param exp boolean weather to use an exponential model istead of a linear model. Default is FALSE.
#' @keywords background, bg, correction, linear fit, exponential fit
#' @export
#' @examples
#' background.correction(d = data, is.bg = bg)



background.correction <- function(d, is.bg, exp = FALSE){
  bground <- clear.outliers(x = d[is.bg], a = 3, use.quartile = T, fill.gaps = T)  #select background data
  t.all <- 1:length(d)
  t <- t.all[is.bg]

  #linear approximation of the background maybe exponential
  bg.fit <- lm(bground ~ if(exp){log(t)}else{t})

  #calculate background values for each data point
  test.frame <- data.frame(t = t.all)
  bg.predict <- predict(object = bg.fit, test.frame)

  #subtract background from signal
  cleared.d <- d - bg.predict

  #return data
  return(cleared.d)
}
