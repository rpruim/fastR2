chol.glht <- confint(glht(chol.model, mcp(trt = "Tukey")))
summary(chol.glht)
plot(confint(chol.glht))

