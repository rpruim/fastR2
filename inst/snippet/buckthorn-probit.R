buck.model2 <- glm(dead~conc,data=buckthorn, 
				   family=binomial(link=probit))
summary(buck.model2)

