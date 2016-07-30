m1 <- lm(strength ~ water, data = concrete) 
anova(m1)
r7 <- lm(strength ~ water + rand(7), data=concrete)
anova(r7)

