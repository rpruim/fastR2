data(cholesterol,package="multcomp")
chol.model <- lm(response~trt,cholesterol)
plot(chol.model, w=c(5,2))       # diagnostic plots
summary(chol.model)
anova(chol.model)

