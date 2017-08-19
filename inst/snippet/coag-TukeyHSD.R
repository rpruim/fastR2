coag.lm <- lm(coag ~ diet, data = coagulation)
TukeyHSD(coag.lm)

