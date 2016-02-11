ut.lm3 <- lm(thermsPerDay ~ temp * kwhpday, ut)
summary(ut.lm3)
xplot(ut.lm3)

