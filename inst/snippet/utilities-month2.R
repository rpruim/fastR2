ut2$monthShifted <- ( ut2$month -2 ) %% 12
ut2.lm2 <- lm(thermsPerDay ~ monthShifted + I(monthShifted^2), ut2)
summary(ut2.lm2)
xyplot(thermsPerDay ~ monthShifted, data=ut2,
                      panel=panel.lm, model=ut2.lm2)
xplot(ut2.lm2, w=1:2)

