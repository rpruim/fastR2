# define several models
concrete.lm0 <- lm(strength ~ limestone + water, data = Concrete)
concrete.lm1 <- lm(strength ~ -1 + limestone + water, data = Concrete)
concrete.lm2 <- lm(strength ~ water, data = Concrete)
concrete.lm3 <- lm(strength ~ limestone, data = Concrete)
concrete.lm4 <- lm(strength ~ 1, data = Concrete)
concrete.lm5 <- lm(strength ~ I(limestone + water), data = Concrete)

