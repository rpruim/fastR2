poison.lm2 <- 
  lm(1/time ~ factor(poison) * factor(treatment), data = Poison)
plot(poison.lm2, w = 1:2)

