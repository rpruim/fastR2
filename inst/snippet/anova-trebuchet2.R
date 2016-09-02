treb.model <-  
  lm(distance ~ projectileWt, data = Trebuchet2)
anova(treb.model)

