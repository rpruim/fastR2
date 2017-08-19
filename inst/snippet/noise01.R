noise.lm <- lm(score ~ noise + group, data = MathNoise)
anova(noise.lm)
favstats(score ~ group, data = MathNoise)

