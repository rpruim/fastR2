# partial regressions of y and x2 on x1
punting.lmy1 <- lm(distance ~ rStrength, data = Punting)
punting.lm21 <- lm(rFlexibility ~ rStrength, data = Punting)
# residuals vs residual
punting.rvr2 <- lm(resid(punting.lmy1) ~ resid(punting.lm21))
# partial regression plots (a.k.a. added-variable plots)
gf_lm(resid(punting.lmy2) ~ resid(punting.lm12)) %>%
  gf_point()
gf_lm(resid(punting.lmy1) ~ resid(punting.lm21)) %>%
  gf_point()

