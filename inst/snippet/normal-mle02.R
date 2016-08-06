MLEs <-
  do(5000) * coef(maxLik(loglik.normal, 
                         start = c(mu = 0, sigma = 1), x = rnorm(40, 100, 10)))
head(MLEs, 3)
histogram( ~ mu,    data = MLEs, width = 0.5, xlab = expression(hat(mu)))
histogram( ~ sigma, data = MLEs, width = 0.5, xlab = expression(hat(sigma)))
xqqmath( ~ mu,    data = MLEs, ylab = expression(hat(mu)))
xqqmath( ~ sigma, data = MLEs, ylab = expression(hat(sigma)))

