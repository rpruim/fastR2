# build two variables that make the model easier to describe 
AirP <- AirP %>% mutate(
  x1 = (loc==1) + 0.5 * (loc==3),
  x2 = (loc==2) + 0.5 * (loc==3))
model3 <- lm(pollution ~ -1 + x1 + x2, data = AirP)
anova(model3, model)

