# regressions of y and x1 on x2
punting.lmy2 <- lm(distance ~ rFlexibility, data = Punting)
punting.lm12 <- lm(rStrength ~ rFlexibility, data = Punting)
# regressions of y and x2 on x1
punting.lmy1 <- lm(distance ~ rStrength, data = Punting)
punting.lm21 <- lm(rFlexibility ~ rStrength, data = Punting)
# these slopes match coefficients from y ~ x1 + x2
coef(lm(resid(punting.lmy2) ~ resid(punting.lm12)))
coef(lm(resid(punting.lmy1) ~ resid(punting.lm21)))
coef(punting.lm)

