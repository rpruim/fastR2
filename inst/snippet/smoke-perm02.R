chisq.test(smokeTab, simulate.p.value = TRUE, B = 10000) %>% 
  pval()
chisq.test(smokeTab, simulate.p.value = TRUE, B = 100000) %>% pval()

