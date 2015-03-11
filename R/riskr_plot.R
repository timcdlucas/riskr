

#' Plot a riskr simulation summary
#'
#'@param f An object of class 'riskr'
#'@'param ... Other plot parameters
#'@name plot.riskr
#'@export

plot.riskr <- function(f, ...){

  mat <- matrix(c(rep(1, length(f$defenderStart)), 2:(length(f$defenderStart) + 1)), nrow = 2 , byrow = TRUE)

  layout(mat)
 

  barplot(table(f$attackerTroopsLeft)/f$sims, xlab = 'Attacking units remaining', ylim = c(0,1), col = grey(0.7))



  for(i in 1:length(f$defenderStart)){

    d <- table(f$defenderTroopsLeft[, i])/f$sims

    
    if(d['0'] == 1){
      clr <- '#CD2626'
    } else {
      clr <- grey(0.8)
    }  

    if(length(d) == 1){
      if(names(d) == '1'){
        d <- c(0, 1)
      } else {
        d <- c(1, 0)
      }
    names(d) <- c(0, 1)
    }      
 

    barplot(d, col = clr,
      xlab = 'Defending units remaining', 
      main = paste('Territory', i))
  }

}



  
