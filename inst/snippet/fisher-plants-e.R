# alternatively, we can do this manually:
o <- counts
e <- theta2probs(theta.hat) * sum(o)
testStats <- c(lrt = 2 * sum( o * log (o/e)), pearson= sum( (o-e)^2/e) )
testStats
1-pchisq(testStats, df = 3-1)

