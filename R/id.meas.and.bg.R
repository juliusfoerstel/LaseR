#' Identify measurement and background
#'
#' This function takes a vector of data, displays it and lets you click on the start and end points of a measurement.
#' It returns a data frame with a column indicating measurement values and one indicating background values
#' @param d Vector of data.
#' @keywords read
#' @export
#' @examples
#' read.exp()



id.meas.and.bg <- function(d, dismiss.beginning = 0, dismiss.after = 0, step = 100){
  x <- 1:length(d)
  y <- d
  xlim <- range(x)
  if(xlim[2] > 600)
    xlim[2] <- 500
  ylim <- range(y)
  
  p_start <- c()
  p_end <- c()
  m <- rep(F,length(x)) #is this a measurement, default no
  bg <- m # not to mess up the plot this is also False by default
  
  newPlot <- function(){
    par(mar = c(4,4,4,4))
    plot(x,y, xlim = xlim, ylim = ylim, pch = 20, ylab = "", xlab = "")
    points(x[p_start], y[p_start], pch = 4, col = 2, cex = 1.5, lwd = 3)
    points(x[p_end], y[p_end], pch = 4, col = 4, cex = 1.5, lwd = 3)
    points(x[m], y[m], col = 3)
    points(x[bg], y[bg], col = 2)
    lims <- par()$usr
    abline(v = lims[1], xpd = T)
    abline(v = lims[2], xpd = T)
    abline(h = lims[3], xpd = T)
    abline(h = lims[4], xpd = T)
    mtext(text = "x-Zoom in", side = 2, line = 3)
    mtext(text = "x-Zoom out", side = 4, line = 3)  
    mtext(text = "y-Zoom in", side = 1, line = 3)
    mtext(text = "y-Zoom out", side = 3, line = 3)
    text( lims[ 2 ], lims[ 3 ], "Done!", adj = c( -0.4, 3 ) , xpd = T, font = 2, col = "darkgreen")#bottom right
    text( lims[ 1 ], lims[ 3 ], "delete\n last", adj = c( 1.4, 1.5 ) , xpd = T, font = 2, col = 2)#bottom left
    text( lims[ 2 ], lims[ 4 ], "move\n right", adj = c( -0.4, -0.5 ) , xpd = T)#top right
    text( lims[ 1 ], lims[ 4 ], "move\n left", adj = c( 1.4, -0.5 ) , xpd = T)#top left
    legend("topright", pch = c(20,4,4), col = c(1,2,4), lwd = c(1,3,3), lty = 0, legend = c("Data", "Start", "End"))
  }
  newPlot()
  
  done <- FALSE
  while(!done){
    where <- mouseclick.where()
    switch(where,
           {#1 delete last
             if(length(p_start)<=length(p_end)){
               p_end <- head(p_end, n = length(p_end)-1)
             } else {
               p_start <- head(p_start, n = length(p_start)-1)
             }
           },
           {#2 y-zoom in
             ylim[2] <- ylim[1] + diff(ylim)*0.9
           },
           {#3 done
             done = TRUE
           },
           {#4 x-zoom in
             xlim[2] <- xlim[1] + diff(xlim)*0.9
           },
           {#5 id points
             s <- 10
             lookup.range <- x > (round(click$x)-s) & x < (round(click$x)+s)
             med.left  <- median(head(y[lookup.range]),s)
             med.right <- median(tail(y[lookup.range]),s)
             s <- 6
             lookup.range <- x > (round(click$x)-s) & x < (round(click$x)+s)
             t <- 0.5
             if(length(p_start)==length(p_end)){
               p_start <- c(p_start,head({x[lookup.range]}[y[lookup.range]>(med.left+t*(med.right-med.left))],1))
             } else {
               p_end <- c(p_end,tail({x[lookup.range]}[y[lookup.range]>(med.left+t*(med.right-med.left))],1))
               xlim <- xlim + p_end[length(p_end)]-xlim[1]
             }
              
           },
           {#6 x-zoom out
             xlim[2] <- xlim[1] + diff(xlim)/0.9
           },
           {#7 move left
             xlim <- xlim - step
           },
           {#8 y-zoom out
             ylim[2] <- ylim[1] + diff(ylim)/0.9
           },
           {#9 move right
             xlim <- xlim + step
           }
    )
    
    newPlot()
  }#end of while
  
  all.points <- c(p_start, p_end)
  m <- rep(F,length(x)) #is this a measurement, default no
  for(i in all.points){
    m[x>i] <- !m[x>i]
  }
  bg <- !m
  m[all.points] <- F
  bg[all.points] <- F
  bg[p_start-1] <- F
  for(i in 1:dismiss.beginning)
    m[p_start+i] <- F
  for(i in 1:dismiss.after)
    bg[p_end+i] <- F
  xlim <- range(x)
  ylim <- range(y)
  newPlot()
  
  
  
  return(data.frame(ms = m, bg = bg))
  
}