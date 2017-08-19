concrete.lm1 <- lm(strength ~ limestone, data = Concrete) 
anova(concrete.lm1)
concrete.r7 <- lm(strength ~ limestone + rand(7), data = Concrete)
anova(concrete.r7)

