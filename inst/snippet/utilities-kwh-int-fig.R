ut.lm3 <- lm(thermsPerDay ~ temp * kwhpday, data = Utilities2)
summary(ut.lm3)
xplot(ut.lm3)

