#' add age column
#' 
#'  @param data Input data
#'        stage At what stage should the ages be calculated? "X","BG","Mean","corr", tbc
#'       
#' @keywords outlier, sigma, range
#' @export
#' @examples
#' add.age.col(data, stage = "mbc)





add.age.col <- function(data, stage = "Mean"){

  col08 <- paste(stage, ".ar08", sep = "") 
  col28 <- paste(stage, ".ar28", sep = "")  
  col48 <- paste(stage, ".ar48", sep = "")
  n <- length(data[,col08])
  ageColName <- paste(stage,".Ages",sep="")
  
  if(stage == "Mean"){
    ageColName <- "AgeFromMean"
  }
  
  ages <- c()
  for(i in 1:n){
    ars <-  c(data[i,col08],data[i,col28],data[i,col48])
    if(sum(is.na(ars))){
      ages[i] <- NA
    }else{
      ages[i] <- golden.age.search(ar08 = data[i,col08], ar28 = data[i,col28], ar48 = data[i,col48],
                                   young.limit = 1,old.limit = 6e5, tolerance = 5)
    }
  }
  
  data[ageColName] <- ages
  return(data)
}