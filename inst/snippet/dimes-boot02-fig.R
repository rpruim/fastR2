Dimes.boot <- 
  do(5000) * c(boot.mean = mean( ~ mass, data = resample(Dimes)))
gf_dhistogram( ~ boot.mean, data = Dimes.boot)
# normality check
gf_qq( ~ boot.mean, data = Dimes.boot)
SE <- sd( ~ boot.mean, data = Dimes.boot); SE
# confidence interval
x.bar + 1.96 * c(0, 1) * SE

