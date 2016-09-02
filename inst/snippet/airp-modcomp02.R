# build a variable that makes the model easier to describe 
AirP <- AirP %>% mutate(x  = (loc == 2) + 0.5 * (loc == 3))
model3 <- lm(pollution ~ 1 + x, data = AirP)
anova(model3, model)

