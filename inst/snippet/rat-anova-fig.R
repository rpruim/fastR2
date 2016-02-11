rat.lm <- lm(consumption~location+flavor, ratpoison)
anova(rat.lm)
plot(rat.lm)

