poison.lm2 <- lm(1/time ~ factor(poison) * factor(treatment), data = Poison)
xplot(poison.lm2, w = c(4, 2))

