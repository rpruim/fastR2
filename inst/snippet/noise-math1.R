model1 <- lm(score ~ noise + group, data = MathNoise)
anova(model1)
favstats(score ~ group, data = MathNoise)

