rat.lm1 <- lm(consumption ~ flavor, data = RatPoison)
anova(rat.lm)
anova(rat.lm1)
summary(rat.lm)$sigma
summary(rat.lm1)$sigma
summary(rat.lm1)$sigma^2/summary(rat.lm)$sigma^2

