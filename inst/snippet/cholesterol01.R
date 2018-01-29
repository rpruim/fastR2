data(cholesterol, package = "multcomp")
cholesterol <- cholesterol %>% 
  mutate(trt = factor(gsub("drug", "", gsub("times*", "x", trt))))
chol.lm <- lm(response ~ trt, data = cholesterol)
plot(chol.lm, w = c(5, 2))       # diagnostic plots
msummary(chol.lm)
anova(chol.lm)

