 normale_multivariee <- function(mean, cov_matrix, n) {
  L <- chol(cov_matrix)
  z <- matrix(rnorm(length(mean) * n), ncol = length(mean))
  print (z)
  
  echantillon <- mean + t(L) %*% t(z)
  
  return(echantillon)
}


mean <- c(1, 1, 2)
cov_matrix <- matrix(c(1, 1, 3, 1, 2, 4, 3, 4, 11), nrow = 3, byrow = TRUE)
print (cov_matrix)
n <- 10000
x <- normale_multivariee(mean, cov_matrix, n)


print("loi normale multivarie:")
rowMeans(x)
print (t(cov(t(x))))

