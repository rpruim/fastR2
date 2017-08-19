punting.lm <- 
  lm(distance ~ rStrength + rFlexibility, data = Punting) 
msummary(punting.lm)
anova(punting.lm)

