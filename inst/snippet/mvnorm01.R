A <- rbind(c(1, 0, 0), c(1, 1, 0), c(2, 1, 1)); A
Sigma <- A %*% t(A); Sigma
mu <- c(0, 1, 2)
mvtnorm::rmvnorm(2, mean = mu, sigma = Sigma)

