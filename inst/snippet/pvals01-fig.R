Pvals.null <- do(10000) * { t.test(rnorm(25, 0, 1)) %>% pval() }
gf_dhistogram(~ p.value, data = Pvals.null, binwidth = 0.02, center = 0.01)
gf_qq(~ p.value, data = Pvals.null, distribution = qunif, geom = "line")

