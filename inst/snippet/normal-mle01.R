x <- rnorm(40, 100, 10)
maxLik(loglik.normal, start = c(mu = 0, sigma = 1), x = x)

