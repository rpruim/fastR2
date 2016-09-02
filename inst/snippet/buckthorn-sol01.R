buck.model <- 
  glm(dead ~ conc, data = Buckthorn, family = binomial)
msummary(buck.model)
dead <- makeFun(buck.model) 

