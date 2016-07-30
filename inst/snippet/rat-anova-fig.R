rat.lm <- lm(consumption ~ location + flavor, data = RatPoison)
anova(rat.lm)
plot(rat.lm)

