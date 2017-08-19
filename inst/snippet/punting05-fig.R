# original regression with two predictors
punting.lm <- lm( distance ~ rStrength + rFlexibility, data = Punting)
# partial regressions of y and x1 on x2
punting.lmy2 <- lm(distance ~ rFlexibility, data = Punting)
punting.lm12 <- lm(rStrength ~ rFlexibility, data = Punting)
# residuals vs residuals
punting.rvr1 <- lm(resid(punting.lmy2) ~ resid(punting.lm12))
# the slope of rvr matches the coefficient from y ~ x1 + x2; intercept is 0
coef(punting.rvr1) %>% round(4)
coef(punting.lm) %>% round(4)
# rvr and original model have the same residuals
gf_point(resid(punting.rvr1) ~ resid(punting.lm))

