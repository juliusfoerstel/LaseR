



mouseclick.where <- function(){
  lims <- par()$usr
  id <- 1
  click <<- locator(1)
  if(click$x > lims[1])
    id <- id+1
  if(click$x > lims[2])
    id <- id+1
  if(click$y > lims[3])
    id <- id+3
  if(click$y > lims[4])
    id <- id+3
  return(id)
}