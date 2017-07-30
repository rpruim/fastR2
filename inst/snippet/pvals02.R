Pvals.alt <- do(10000) * { t.test(rnorm(25, 1/2, 1)) %>% pval() }
gf_dhistogram(~ p.value, data = Pvals.alt, binwidth = 0.01, center = 0.005)
gf_qq(~ p.value, data = Pvals.alt, distribution = qunif, geom = "line")

