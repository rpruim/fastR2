anova(lm(strength ~ limestone * water, concrete))
anova(lm(strength ~ water * limestone, concrete))
anova(lm(strength ~ limestone:water + limestone + water, concrete))
anova(lm(strength ~ limestone:water +  water + limestone, concrete))

