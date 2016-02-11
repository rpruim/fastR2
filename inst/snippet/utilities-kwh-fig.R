ut.lm2 <- lm(thermsPerDay ~ temp + kwhpday, ut)
summary(ut.lm2)
xplot(ut.lm2)

