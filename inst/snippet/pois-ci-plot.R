plot(ml10) + 
  geom_abline(slope = 0, intercept = logLik(ml10) - 1.96, 
              linetype = "dashed")

