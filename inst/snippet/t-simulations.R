# an example CI from a sample of size 20
confint(t.test(rnorm(20, 500, 100)))
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, estimand = 500, 
	rdist = rnorm, args = list(mean = 500, sd = 100))

