pheno.lm <- lm(log(weight) ~ log(waist) + log(height), data = Pheno)
summary(pheno.lm)

