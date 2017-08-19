# simulate 100 intervals and plot them. 
CIsim(n = 20, samples = 100, estimand = 500, 
      rdist = rnorm, args = list(mean = 500, sd = 100),
      method = zci, method.args = list(sd = 100))

