airp.lm4 <- lm(pollution~location,airpollution)
contr4 <- mcp(location = rbind(
	"hill - plains" = c(1,-1,0)))
summary(glht(airp.lm4,contr4))

