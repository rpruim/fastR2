model <- aov(sqrt(trapped) ~ color, data = Bugs)
TukeyHSD(model)
model <- lm(sqrt(trapped) ~ color, data = Bugs)
summary(glht(model,mcp(Color="Tukey")))

