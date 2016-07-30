plot(ml.pois10) + 
  geom_abline(slope = 0, intercept = logLik(ml.pois10) - 1.96, 
              linetype = "dashed")

