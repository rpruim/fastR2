favstats(gpigs)
n <- length(gpigs); x.bar <- mean(gpigs); s <- sd(gpigs)
GP.boot <- 
  (do(5000) * favstats(resample(gpigs))) %>%
  mutate(t = (mean - x.bar) / (sd / sqrt(n)))
# percentile interval
cdata( ~ mean, data = GP.boot)
# normalized bias
(mean( ~ mean, data = GP.boot) - x.bar) / sd( ~ mean, data = GP.boot)

# bootstrap-t interval
q <- quantile( ~ t, data = GP.boot, p = c(0.025, 0.975)); q
x.bar - q[2:1] * s/sqrt(n)

