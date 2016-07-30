mu <- 0.4 / (0.4 + 0.6); mu           # mean for beta dist
v <- (0.4 * 0.6) / ((0.4 + 0.6)^2 * (0.4 + 0.6 + 1)); v  # var for beta dist
#
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, 
      rdist = rbeta, args = list(shape1 = 0.4, shape2 = 0.6),
	    estimand = mu, method = zci, method.args = list(sd = sqrt(v)))

