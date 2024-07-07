generate_normal <- function(n, mean, stdev) {
  u1 <- runif(n)
  u2 <- runif(n)
  
  z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)
  
  result <- c(z1, z2)
  return((result * stdev) + mean)
}

n <- 10000
mean_value <- 2
stdev_value <- 2

samples <- generate_normal(n, mean_value, stdev_value)


######################################################
#histogram de lois normal
hist(samples, breaks = 10, main = "Simulation d'une lois normal mean=2 stdev^2=4")



###############################HYPEREXPO#########################################################

generer_N <- function(n, probabilities) {
  u1 <- runif(n)
  probab_cumul <- cumsum(probabilities)
  N <- rep(0, n)
  
  for (i in 1:n) {
    N[i] <- sum(probab_cumul < u1[i]) + 1
  }
  
  return(N)
}

generate_exp <- function(u, N) {
  rates <- ifelse(N == 1, 1, 2)
  return(rexp(length(N), rate = rates))
}


n <- 1000
probabilities <- c(0.25, 0.75)
N <- generer_N(n, probabilities)
u2 <- runif(n)
X <- generate_exp(u2, N)

cat("generated N values :", N, "\n")
cat("generated X values:", X, "\n")






generate_hyperexp(n,)























##################################POISSON######################################################

generate_poisson <- function(n, lambda) {
  echant_poisson <- numeric(n)
  
  for (i in 1:n) {
    u <- runif(1) 
    somme_exp <- 0
    k <- 0
    
    while (somme_exp <= 1) {
      k <- k + 1
      somme_exp <- somme_exp -log(runif(1)) /lambda 
    }
    
    echant_poisson[i] <- k - 1
  }
  
  return(echant_poisson)
}


n <- 10000
lambda1 <- 5


echantillon1 <- generate_poisson(n, lambda1)

generate_poisson_rpois <- function(n, lambda) {
  return(rpois(n, lambda))
}

echantillon1
hist(echantillon1, breaks = 10, main = "Simulation d'une lois poisson lamda=5")






















