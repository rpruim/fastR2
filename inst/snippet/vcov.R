treb.model <-  
  lm(distance ~ projectileWt, data = Trebuchet2)
vcov(treb.model) 
sqrt(diag(vcov(treb.model)))
treb.model %>% summary() %>% coef()

