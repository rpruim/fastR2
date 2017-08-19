pheno.lm <- 
  lm(log(weight) ~ log(waist) + log(height), data = Pheno)
msummary(pheno.lm)

