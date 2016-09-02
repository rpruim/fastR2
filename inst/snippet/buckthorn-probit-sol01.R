buck.model2 <- 
  glm(dead ~ conc, data = Buckthorn, family = binomial(link = probit))
msummary(buck.model2)

