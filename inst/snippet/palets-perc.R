anova(lm(perc ~ employee, data = Pallets2))
confint(glht(lm(perc ~ employee, data = Pallets2), 
             mcp(employee = "Tukey")))

