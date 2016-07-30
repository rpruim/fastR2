# an example CI from a sample of size 20
confint(t.test(rt(20, 3)))
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, estimand = 0, 
      rdist = rt, args = list(df = 3))

