coag.altmodel <- lm(coag ~ -1 + diet, data = coagulation)
msummary(coag.altmodel)

