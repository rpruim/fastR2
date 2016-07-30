Soap.model2 <- lm(I(weight^(1/3)) ~ day, data = Soap)
msummary(Soap.model2)

