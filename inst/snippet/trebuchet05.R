treb.model <-  
  lm(distance ~ projectileWt, data = Trebuchet2)
treb.dist <- makeFun(treb.model)
treb.dist(projectileWt = 44)
treb.dist(projectileWt = 44, interval = "confidence")
treb.dist(projectileWt = 44, interval = "prediction")

