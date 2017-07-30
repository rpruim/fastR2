lrt.stat <- 2 * (logLik(ml) - logLik(ml0)) %>% as.vector() 
lrt.stat
1 - pchisq(lrt.stat, df = 1)     # p-value based on asymptotic distribution

