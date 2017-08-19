treb.model <- lm(distance ~ projectileWt, data = Trebuchet2) 
coef(treb.model)
gf_point(distance ~ projectileWt, data = Trebuchet2) %>%
  gf_lm()

