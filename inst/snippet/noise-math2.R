model2 <- lm(score ~ noise * group, data = MathNoise)
anova(model2)

