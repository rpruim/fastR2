noise.lm2 <- lm(score ~ noise * group, data = MathNoise)
anova(noise.lm2)

