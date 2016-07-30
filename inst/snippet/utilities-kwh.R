ut.lm2 <- lm(thermsPerDay ~ temp + kwhpday, data = Ut)
summary(ut.lm2)
xplot(ut.lm2)

