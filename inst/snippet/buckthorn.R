buck.model <- glm(dead ~ conc, data = Buckthorn, family = binomial)
summary(buck.model)
dead <- makeFun(buck.model) 

