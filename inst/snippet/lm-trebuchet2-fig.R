treb.model <- lm(distance ~ projectileWt, data = Trebuchet2)
coef(treb.model)
xyplot(distance ~ projectileWt, data = Trebuchet2, type = c('p', 'r'))

