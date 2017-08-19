A <- rbind(c(1, 0), c(-1, 0))
Sigma <- A %*% t(A); Sigma
det(Sigma)
rho <- Sigma[1,2] / (Sigma[1,1] * Sigma[2,2]); rho

