model <- aov(sqrt(trapped) ~ color, data = Bugs)
TukeyHSD(model)
model <- lm(sqrt(trapped) ~ color, data = Bugs)
glht(model, mcp(color = "Tukey")) %>%
  summary()          

