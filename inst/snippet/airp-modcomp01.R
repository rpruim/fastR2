# convert location to a numeric variable for convenience
AirP <- AirPollution %>%
  mutate(loc = as.numeric(location))
model <- lm(pollution ~ location, data = AirP)
model2 <- lm(pollution ~  1 + (loc == 3), data = AirP)
anova(model2, model)

