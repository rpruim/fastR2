x <- c(20, 24, 27, 28, 28, 28, 29, 30,  
       30, 30, 30, 32, 33, 34, 35, 38)
NormalGrid <- 
  expand.grid(
    mu = seq(20, 40, length.out = 200),
    sigma = seq(0.1, 15, length.out = 200)   # avoid sigma = 0 here
  ) %>%
  mutate(
    prior = dnorm(mu, 0, 20) * dgamma(sigma, shape = 3, rate = 1/3),
    likelihood = mapply(
      function(m, s) {prod(dnorm(x, mean = m, sd = s))}, 
      m = mu, s = sigma),
    posterior = prior * likelihood
  )
NormalGrid %>% 
  arrange(-posterior) %>%
  head(3)

