# an example CI (n = 20; mu = 10)
confint(t.test(rexp(20, 1/10)))
# 10,000 simulated samples of sizes 2, 5 and 20
CIsim(n = c(2, 5, 20), samples = 10000, estimand = 10, 
      rdist = rexp, args = list(rate = 1/10))

