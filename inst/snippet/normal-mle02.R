MLEs <-
  do(5000) * coef(maxLik(loglik.normal, 
                         start = c(mu = 0, sigma = 1), 
                         x = rnorm(30, 100, 10)))
head(MLEs, 3)
gf_dhistogram( ~ mu,    data = MLEs, binwidth = 0.5) %>%
  gf_labs(x = expression(hat(mu)))
gf_dhistogram( ~ sigma, data = MLEs, binwidth = 0.5) %>%
  gf_labs(x = expression(hat(sigma)))
gf_qq( ~ mu, data = MLEs, geom = "line") %>%
  gf_labs(y = expression(hat(mu)))
gf_qq( ~ sigma, data = MLEs, geom = "line") %>%
  gf_labs(y = expression(hat(sigma)))

