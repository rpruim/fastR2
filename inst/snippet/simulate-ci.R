# an example CI from a sample of size 20
zci(rnorm(20, 500, 100))
# 10,000 simulated samples each of size 2, 5 and 20
CIsim(n = c(2, 5, 20), samples = 10000, rdist = rnorm, 
      args = list(mean = 500, sd = 100),
	    estimand = 500, method = zci, method.args = list(sd = 100))

