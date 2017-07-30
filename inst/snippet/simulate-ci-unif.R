mu <- 1/2; v <- 1/12           # mean and variance
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, rdist = runif, estimand = mu,
	method = zci, method.args = list(sd = sqrt(v)))

