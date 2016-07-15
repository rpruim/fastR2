x <- 35; n <- 55
pi.hat <- x / n; pi.hat
SE <- sqrt(pi.hat * (1 - pi.hat) / n); SE
pi.hat + c(-1,1) * qnorm(0.975) * SE

