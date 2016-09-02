pheno.male <- 
  lm(log(weight) ~ log(waist) + log(height), data = Pheno %>% filter(sex == "M"))
pheno.female <- 
  lm(log(weight) ~ log(waist) + log(height), data = Pheno %>% filter(sex == "F"))
msummary(pheno.male)
msummary(pheno.female)
plot(pheno.male)     # males only
plot(pheno.female)   # females only
plot(pheno.lm)       # all subjects

