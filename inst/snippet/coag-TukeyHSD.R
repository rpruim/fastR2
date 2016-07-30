coag.model <- lm(coag ~ diet, data = coagulation); coag.model
TukeyHSD(coag.model)

