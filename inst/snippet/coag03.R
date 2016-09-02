coag.lm  <- lm(coag ~ diet, data = coagulation)
coag.lm1 <- lm(coag ~ 1,    data = coagulation)
anova(coag.lm1, coag.lm)

