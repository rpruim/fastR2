Ut2$monthShifted <- (Ut2$month - 2) %% 12
Ut2.lm2 <- lm(thermsPerDay ~ monthShifted + I(monthShifted^2), data = Ut2)
summary(Ut2.lm2)
plotModel(Ut2.lm2)
xplot(Ut2.lm2, w = 1:2)

