require(multcomp)
coag.glht <- glht(coag.model, mcp(diet = "Tukey"))
summary(coag.glht)  
mplot(TukeyHSD(coag.model)) 
plot(confint(coag.glht))

