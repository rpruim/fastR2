poison.lm <- 
  lm(time ~ factor(poison) * factor(treatment), data = Poison)
anova(poison.lm)

