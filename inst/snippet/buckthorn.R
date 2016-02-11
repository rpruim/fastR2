buck.model <- glm(dead~conc,data=buckthorn, family=binomial)
summary(buck.model)
dead <- makeFun(buck.model)

