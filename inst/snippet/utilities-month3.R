Ut2.lm3 <- lm(thermsPerDay ~ poly(monthShifted, 2), data = Ut2)
summary(Ut2.lm3)
quantile(fitted(Ut2.lm2) - fitted(Ut2.lm3))

