# using mcp() to help build the contrasts
airp.lm3 <- lm(pollution ~ location, data = AirPollution)
contr3 <- mcp(location = rbind(
	"hill - plains" = c(1, -1, 0),
	"suburb - urban" = c(1, 1, -2)
	))
msummary(glht(airp.lm3, contr3))

