plot(ml.binom) +   
  geom_abline(slope = 0, intercept = logLik(ml.binom) - 1.96, 
              linetype = "dashed") +
  ylim(-42, -35.5) + 
  labs(title = "parameter: proportion")
plot(ml.binom2) +
  geom_abline(slope = 0, intercept = logLik(ml.binom2) - 1.96, 
              linetype = "dashed") +
  ylim(-42, -35.5) + 
  labs(title = "parameter: odds")
plot(ml.binom3) +
  geom_abline(slope = 0, intercept = logLik(ml.binom3) - 1.96, 
              linetype = "dashed") +
  ylim(-42, -35.5) + 
  labs(title = "parameter: log odds")

