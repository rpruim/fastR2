pheno.lm <- lm(log(weight) ~ log(waist) + log(height), pheno)
summary(pheno.lm)

