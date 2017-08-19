A <- rbind(c(1, 0, 0), c(1, 1, 0), c(1, 2, 1))
# covariance matrix
Sigma<- A %*% t(A); Sigma
# marginal covariance matrix
Sigma[-3, -3]

