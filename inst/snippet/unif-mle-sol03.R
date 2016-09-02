# a graph of the likelihood function shows why
theta <- seq(6, 12, by = 0.002)
y1 <- sapply(theta, function(theta) { lik.unif(theta, x)} )
y2 <- sapply(theta, function(theta) { loglik.unif(theta, x)} )
xyplot(y1 ~ theta,
    xlab = expression(theta),
    ylab = "likelihood", cex = 0.5)
xyplot(y2 ~ theta,
    xlab = expression(theta),
    ylab = "log-likelihood", cex = 0.5)

