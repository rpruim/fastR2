pheno.case <- lm(log(weight) ~ log(waist) + log(height),
                 data = Pheno %>% filter(t2d == "case"))
pheno.control<- lm(log(weight) ~ log(waist) + log(height),
                 data = Pheno %>% filter(t2d == "control"))
msummary(pheno.case)
msummary(pheno.control)

