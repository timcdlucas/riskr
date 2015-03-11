



#' Print a riskr object nicely
#'
#'@name print.riskr
#'@export

print.riskr <- function(f){

  if(length(f$defenderStart) == 1){
    cat(paste('Risk battle odds for', f$attackerStart, 'attackers and', f$defenderStart, 'defenders:\n'))
  } else {
    cat(paste('Risk battle odds for', f$attackerStart, 'attackers and\n', sum(f$defenderStart), 'defenders in', length(d), 'territories:\n' ))
  }
  
  cat('\n')

  message("Attacker survives ", 100 * sum(f$attackerTroopsLeft > 0) / f$sims, "%") 

  if(sum(f$attackerTroopsLeft > 0) / f$sims > 0.5){
    message("Attacker average loses: ", round(f$attackerStart - mean(f$attackerTroopsLeft), 2))
  } else {
    message("Defender average loses: ", round(sum(f$defenderStart) - mean(rowSums(f$defenderTroopsLeft)), 2))
  }
    
  if(length(f$defenderStart) > 1 & mean(f$attackerTroopsLeft == 0) > 0.5){
    taken <- median(rowSums(f$defenderTroopsLeft == 0))
    message("Attacker average countries taken: ", taken)
  }

  cat(paste('\n\nFrom', f$sims, 'simulations.\n'))
}
