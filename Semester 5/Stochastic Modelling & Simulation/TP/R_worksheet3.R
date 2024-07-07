
CLG <- function(m, a, b, y0, n) {
  y <- y0
  for (i in 2:n) {
    y[i] = (a * y[i-1] + b) %% m
  }
  return(y/m)
}


X <- CLG(10^3, 121, 567, 0, 10000)

m <- 10 
L <- seq(from=0, to=1,by=1/m) 
V <- c()

for (i in 1:(length(L)-1)){
  V[i]<- sum(X> L[i]& X< L[i+1])
}
print(V)

ifelse(sum(V-(length(X)/m)^2)/(length(X)/m) < qchisq(0.95,m-1),
       paste("La suite est bien aleatoire"),
       paste("La suite n'est pas bien aleatoire"))


uniform_data <- runif(10000)
hist(X, main = "Comparaison avec une loi uniforme", xlab = "Classes", ylab = "Fréquence", col = "lightblue", border = "black", nclass=10)

hist(uniform_data, col = "lightgreen", add = TRUE, nclass=10)

vecteur_aleatoire <- CLG(10^3, 121, 567, 0, 20)
vecteur_aleatoire

vecteur_aleatoire2 <- CLG(10^3, 121, 567, 1, 20)
vecteur_aleatoire2

