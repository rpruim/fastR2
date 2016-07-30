data(rat, package = "alr3")
rat.lm <- lm(y ~ BodyWt * LiverWt, data = rat)
summary(rat.lm)

