o <- geno
e <- theta2probs(theta.hat) * sum(o); e
testStats <- c(lrt = 2 * sum( o * log (o/e)), pearson= sum( (o-e)^2/e) )
testStats
1-pchisq(testStats, df = 2-1)

