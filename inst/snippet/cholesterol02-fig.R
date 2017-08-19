chol.glht <- confint(glht(chol.lm, mcp(trt = "Tukey")))
msummary(chol.glht)
plot(confint(chol.glht))

