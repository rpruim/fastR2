Dimes.boot <- 
  do(5000) * c(boot.mean = mean( ~ mass, data = resample(Dimes)))
gf_dhistogram( ~ boot.mean, data = Dimes.boot)

