concrete.lm0 <- lm(strength ~ limestone + water, data = concrete)
concrete.lm1 <- lm(strength ~ limestone, data = concrete)
concrete.lm2 <- lm(strength ~ water, data = concrete)
concrete.lm3 <- lm(strength ~ 1, data = concrete)
concrete.lm4 <- lm(strength ~ I(limestone + water), data = concrete)
concrete.lm5 <- lm(strength ~ -1 + limestone + water, data = concrete)

