punting.lm3 <-
  lm(distance ~ rStrength + rFlexibility, Punting[-3, ])
punting.lm %>% summary() %>% coef()
punting.lm3 %>% summary() %>% coef()
car::avPlots(punting.lm3, id.n = 1)

