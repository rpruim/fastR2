gf_point(distance ~ projectileWt, data = Trebuchet2) %>%
  gf_lm() %>%
  gf_ribbon(
    lwr + upr ~ projectileWt, fill = "skyblue",
    data = cbind(Trebuchet2, predict(treb.model, interval = "prediction"))
    ) %>%
  gf_ribbon(
    lwr + upr ~ projectileWt, 
    data = cbind(Trebuchet2, predict(treb.model, interval = "confidence"))) 
# simpler way, using gf_lm()
gf_point(distance ~ projectileWt, data = Trebuchet2) %>%
  gf_lm(interval = "prediction", fill = "skyblue") %>%
  gf_lm(interval = "confidence")

