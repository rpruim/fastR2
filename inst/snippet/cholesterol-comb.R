model1 <- lm(response ~ trt, data = cholesterol)
cholesterol <- 
  cholesterol %>% 
  mutate(x1 = trt == "drugD", x2 = trt == "drugE")
model2 <- lm(response~ 1 + x1 + x2 , cholesterol)
anova(model1, model2)

