ut2.lm3 <- lm(thermsPerDay ~ poly(monthShifted, 2), ut2)
summary(ut2.lm3)
quantile( fitted(ut2.lm2) - fitted(ut2.lm3) )

