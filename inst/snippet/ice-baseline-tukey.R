require(multcomp) 
confint(glht(base.lmadd, mcp(treatment = "Tukey")), level = 0.9)

