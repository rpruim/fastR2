airp.lm <- lm(pollution ~ location, data = AirPollution)
TukeyHSD(airp.lm)

