#1
#Simulates a fair coin flip game 
simulate_coin_flip <- function(n) {
  heads <- 0
  tails <- 0
  
  for (i in 1:n) {
    result <- sample(c("Pile", "Face"), size = 1)
    if (result == "Pile") {
      heads <- heads + 1
    } else {
      tails <- tails + 1
    }
  }
  
  return(c(Pile = heads, Face = tails))
}

#diffrent experiences
n_values <- c(5, 10, 50, 100, 10^3, 10^4, 10^5, 10^6)

#execution
for (n in n_values) {
  results <- simulate_coin_flip(n)
  cat(sprintf("-->Nombre de lancers : %d, Pile : %d, Face : %d\n", n, results["Pile"], results["Face"]))
  cat(sprintf("Frequence de Pile : %.3f , Frequence de Face : %.3f\n",results["Pile"]/n,results["Face"]/n ))
}

#sol
PF <- function(n, p) {
  random_numbers <- runif(n)
  results <- ifelse(random_numbers < p, "Face", "Pile")
  return(results)
}

game_results <- PF(10, 0.8)
print(game_results)

#2
Game <- function (probability, gain, loss, initial_fortune, target_fortune){
  fortune <- initial_fortune
  rounds <- 0
  
  while (fortune > 0 & fortune < 80) {
    result <- ifelse (runif(1) < probability, gain, -loss)
  }
  
}



