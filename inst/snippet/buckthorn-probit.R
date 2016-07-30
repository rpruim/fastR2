buck.model2 <- 
  glm(dead ~ conc, data = Buckthorn, family = binomial(link = probit))
summary(buck.model2)

