plot(ml.binom, hline = TRUE)    
  ylim(-42, -35.5) + 
  labs(title = "parameter: proportion")
plot(ml.binom2, hline = TRUE) +
  ylim(-42, -35.5) + 
  labs(title = "parameter: odds")
plot(ml.binom3, hline = TRUE) +
  ylim(-42, -35.5) + 
  labs(title = "parameter: log odds")

