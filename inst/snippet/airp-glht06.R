airp.lm4 <- lm(pollution ~ location, data = AirPollution)
contr4 <- mcp(location = rbind(
	"hill - plains" = c(1, -1, 0)))
msummary(glht(airp.lm4, contr4))

