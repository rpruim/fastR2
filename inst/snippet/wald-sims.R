WaldSims <- 
  CIsim(2000, n = c(5, 10, 20, 40), 
        method = binom.test, method.args = list(ci.method = "Wald"),
        rdist = rbinom, args = list(size = 1, prob = 0.2),
        estimand = 0.2)

