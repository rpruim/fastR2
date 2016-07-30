ml.binom <- maxLik2(loglik.binom, x = 35, n = 55, start = 0.5) 
plot(ml.binom) + 
  geom_abline(slope = 0, intercept = logLik(ml.binom) - 1.96, linetype = "dashed")

