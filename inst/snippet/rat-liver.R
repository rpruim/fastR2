data(rat, package = "alr4")  
rat.lm <- lm(y ~ BodyWt * LiverWt, data = rat)
msummary(rat.lm)

