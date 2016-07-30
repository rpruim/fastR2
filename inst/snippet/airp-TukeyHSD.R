airp.model <- lm(pollution ~ location, data = AirPollution)
TukeyHSD(airp.model)

