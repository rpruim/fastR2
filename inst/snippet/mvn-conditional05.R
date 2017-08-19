Sigma
mu <- c(1, 2, 3); mu
# means
mu[2:3] + Sigma[2:3, 1] %*% solve(Sigma[1, 1]) %*% (3 - mu[1])
# variance-covariance
Sigma[2:3, 2:3] - Sigma[2:3, 1] %*% solve(Sigma[1, 1]) %*% Sigma[1, 2:3]


