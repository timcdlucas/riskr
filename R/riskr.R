
#'A package to calculate Hasbro Risk battle odds.
#'
#'@name riskr
#'@author Tim CD Lucas
#'@docType package
#'@import assertthat RColorBrewer

NULL





#' Simulate a risk campaign
#'
#' Given a succession of defending countries, what are your odds of winning and how far will you get?
#'
#'@param a The number of attacking units (remember to minus one to be left behind).
#'@param d A vector giving the number of defending units in successive defenders countries.
#'
#'@name fight
#'@export

fight <- function(a, d, sims = 1000){
  is.count(a)
  assert_that(is.numeric(d))
  # Check defence is pretty much integers
  assert_that(all(d == as.integer(d)))
  assert_that(all(d > 0))

  d <- as.integer(d)

  # preroll dice. 
  #   Rough upper bound. At least 1 unit dies each turn. At most 5 rolls per turn.
  dice <- sample(1:6, sims * 10 * ((sum(d) + a)), replace = TRUE)
  count <- 1

  # Make empty matrices to record results
  aRecord <- rep(NA, sims)
  dRecord <- matrix(NA, ncol = length(d), nrow = sims)

  # loop through sims
  for(s in 1:sims){
    simOut <- runSim(a, d, count, dice)
    count <- simOut$count
    aRecord[s] <- simOut$a
    dRecord[s, ] <- simOut$d
  }
  
  message("Attacker wins ", 100 * sum(aRecord > 0) / sims, "%") 

  if(sum(aRecord > 0) / sims > 0.5){
    message("Attacker average loses: ", round(a - mean(aRecord), 2))
  } else {
    message("Defender average loses: ", round(sum(d) - mean(rowSums(dRecord)), 2))
  }
    
  if(length(d) > 1 & mean(aRecord == 0) > 0.5){
    taken <- median(rowSums(dRecord == 0))
    message("Attacker average countries taken: ", taken)
  }

  results <- list(attackerTroopsLeft = aRecord, defenderTroopsLeft = dRecord)
  class(results) <- 'riskr'

  return(invisible(results))

}



# Function to run one simulation

runSim <- function(a, d, count, dice){
  # Run a simulation
  while(a > 0 & sum(d) > 0){

    # which defending country have we reached
    dCountry <- which(d != 0)[1]
    
    # Work out what dice to roll
    aDice <- if(a == 1){
               1
             } else if(a == 2){
               2
             } else if(a > 2){
               3
             }

    dDice <- if(d[dCountry] == 1){
               1
             } else if(d[dCountry] > 1){
               2
             }

    # Roll dice.
    dRoll <- sort(dice[count:(count + dDice - 1)], decreasing = TRUE)
    count <- count + dDice

    aRoll <- sort(dice[count:(count + aDice - 1)], decreasing = TRUE)
    count <- count + dDice

    minDice <- min(aDice, dDice)
    dDeaths <- sum(aRoll[1:minDice] > dRoll[1:minDice])
    aDeaths <- minDice - dDeaths
  
    d[dCountry] <- d[dCountry] - dDeaths
    # If you fully defeat a country (except the final country), leave 1 attacker behind.   
    #print(c(d, d[dCountry], dCountry ))
    #print(c(aDeaths, dDeaths, minDice))
    if(d[dCountry] == 0 & dCountry != length(d) & a != 0){
      a <- a - 1
    }
    a <- a - aDeaths


  }

  return(list(a = a, d = d, count = count))

}


