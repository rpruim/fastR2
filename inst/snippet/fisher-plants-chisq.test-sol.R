chisq.test(fisher.counts, p = theta2probs(theta.hat))
# so we can grab that statistic and redo the p-value:
X <- stat(chisq.test(fisher.counts, p = theta2probs(theta.hat))); X
1 - pchisq(X, df = 2)

