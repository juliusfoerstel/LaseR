#' Plot information from Laser Experiment
#'
#' This function plots a laser experiment
#' @keywords laser info, plot
#' @param data.table data frame of laser info
#' @param highlight id of track to be highlighted
#' @export
#' @examples
#' plot.laserexperiment()


plot.laserexperiment <- function(data.table = NA, highlight = 0){
  
  plot(0, 
       xlim = c(min(c(data.table$start.x,data.table$end.x),na.rm = T),max(c(data.table$start.x,data.table$end.x),na.rm = T)), 
       ylim = c(min(c(data.table$start.y,data.table$end.y),na.rm = T),max(c(data.table$start.y,data.table$end.y),na.rm = T)),
       type = "n", xlab = "X [mm]", ylab = "Y [mm]")
  
  if(sum(data.table$Pattern.Type == 7)){
  points(x = data.table$X[data.table$Pattern.Type == 7], 
         y = data.table$Y[data.table$Pattern.Type == 7], 
         pch = 20, cex = 1, col = 4)
  text(x = data.table$X[data.table$Pattern.Type == 7], 
       y = data.table$Y[data.table$Pattern.Type == 7], 
       labels = data.table$Caption[data.table$Pattern.Type == 7], 
       pos = 1,col = 4, cex = 0.4, offset = 0.2)
  }
  
  
  if(sum(data.table$Pattern.Type == 4)){
    
  arrows(x0 = data.table$start.x[data.table$Pattern.Type == 4], 
         y0 = data.table$start.y[data.table$Pattern.Type == 4],
         x1 = data.table$end.x[data.table$Pattern.Type == 4],
         y1 = data.table$end.y[data.table$Pattern.Type == 4], 
         pch = 20, cex = 0.5,angle = 10,length = 0.1)
  
  text(x = data.table$X[data.table$Pattern.Type == 4], y = data.table$Y[data.table$Pattern.Type == 4], labels = data.table$Caption[data.table$Pattern.Type == 4], pos = 2, cex = 0.4, offset = 0.2)
  }
  
  
  if(highlight){
    
    arrows(x0 = data.table$start.x[highlight], 
           y0 = data.table$start.y[highlight],
           x1 = data.table$end.x[highlight],
           y1 = data.table$end.y[highlight], 
           pch = 20, cex = 0.5,angle = 10,length = 0.1, lwd = 2, col = 2)
    
    text(x = data.table$X[highlight], y = data.table$Y[highlight], labels = data.table$Caption[highlight],  col = 2, pos = 2, cex = 0.4, offset = 0.2)
    
  }
}
