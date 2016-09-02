require(multcomp) 
airp.cint <- confint(glht(airp.lm, mcp(location = "Tukey")))
airp.cint  
plot(airp.cint)
mplot(TukeyHSD(airp.lm))  

