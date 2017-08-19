gf_dhistogram( ~ fitted(pheno.lm))
gf_boxplot(resid(pheno.lm) ~ ntiles(fitted(pheno.lm), 10)) %>%
  gf_labs(x = "fitted value deciles")

