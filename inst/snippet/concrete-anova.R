anova(lm(strength ~ limestone * water, data = Concrete))
anova(lm(strength ~ water * limestone, data = Concrete))
anova(lm(strength ~ limestone:water + limestone + water, data = Concrete))
anova(lm(strength ~ limestone:water +  water + limestone, data = Concrete))

