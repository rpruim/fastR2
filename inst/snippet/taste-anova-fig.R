favstats(score~type, data=tastetest)
xyplot(score~type,data=tastetest)
taste.lm <- lm(score~type,data=tastetest)
anova(taste.lm)
taste.cint <- confint(glht(taste.lm,mcp(type="Tukey"))); taste.cint
plot(taste.cint)

