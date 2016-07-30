require(multcomp)
airp.cint <- confint(glht(airp.model, mcp(location = "Tukey")))
airp.cint  
plot(airp.cint)
mplot(TukeyHSD(airp.model))  

