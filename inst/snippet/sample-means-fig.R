# 5000 sample means of samples of size 16 from N(100, 12):
SamplingDist <- 
  do(5000) * c(sample.mean = mean(rnorm(16, 100, 12)))
mean(~ sample.mean, data = SamplingDist)
sd( ~ sample.mean, data = SamplingDist)
gf_dhistogram( ~ sample.mean, data = SamplingDist, 
               bins = 20, alpha = 0.5) %>%
  gf_vline(xintercept = 100) %>%
  gf_function(fun = dnorm, args = list(mean = 100, sd = 3))

gf_qq( ~ sample.mean, data = SamplingDist)

