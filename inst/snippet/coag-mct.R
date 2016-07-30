coag.model <- lm(coag ~ diet, data = coagulation)
coag.model1 <- lm(coag ~ 1, data = coagulation)
anova(coag.model1, coag.model)

