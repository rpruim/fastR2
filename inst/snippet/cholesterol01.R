data(cholesterol, package = "multcomp")
chol.lm <- lm(response ~ trt, data = cholesterol)
plot(chol.lm, w = c(5, 2))       # diagnostic plots
msummary(chol.lm)
anova(chol.lm)

