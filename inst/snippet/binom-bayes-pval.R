1- pbeta(0.5, 38 + 1, 62 + 1)          # 1-sided Bayesian p-value
pval(binom.test(38, 100, alt = "less"))      # for comparison
pval(prop.test(38, 100, alt = "less"))       # for comparison

