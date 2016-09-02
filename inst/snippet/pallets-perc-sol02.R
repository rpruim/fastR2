anova(lm(perc ~ employee + day, data = Pallets2))
confint(glht(lm(perc ~ employee + day, data = Pallets2), mcp(employee = "Tukey")))
plot(confint(glht(lm(perc ~ employee + day, Pallets2), mcp(employee = "Tukey"))))

