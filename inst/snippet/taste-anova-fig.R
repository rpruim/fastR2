favstats(score ~ type, data = TasteTest)
xyplot(score ~ type, data = TasteTest)
taste.lm <- lm(score ~ type, data = TasteTest)
anova(taste.lm)
taste.cint <- confint(glht(taste.lm, mcp(type = "Tukey"))); taste.cint
plot(taste.cint)

