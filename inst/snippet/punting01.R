punting.lm <- 
  lm(distance ~ rStrength + rFlexibility, data = Punting) 
summary(punting.lm)
anova(punting.lm)

