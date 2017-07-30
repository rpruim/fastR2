B <- runif(10000, 10150, 10250)
Dimes.boot <- do(10000) * mean( ~ mass, data = resample(Dimes))
head(Dimes.boot, 3)
Dimes.boot <- 
  Dimes.boot %>% mutate(D = mean, N = B / D)
gf_dhistogram( ~ N, data = Dimes.boot)
gf_qq( ~ N, data = Dimes.boot)
sd( ~ N, data = Dimes.boot)

