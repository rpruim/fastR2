pheno.plain <- lm(weight ~ waist + height, data = Pheno)
c(plain = rsquared(pheno.plain), transformed = rsquared(pheno.lm))
c(plain = AIC(pheno.plain), transformed = AIC(pheno.lm))
plot(pheno.plain, w = 2)
plot(pheno.lm, w = 2)

