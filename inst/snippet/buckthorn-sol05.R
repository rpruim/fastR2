observed <- tbl[, 3:4]; observed 
expected <- tbl[, 6:7]; expected
lrt <- 2 * sum(observed *  log (observed / expected)); lrt
pearson <- sum((observed - expected)^2 / expected); pearson
# pvals
1 - pchisq(pearson, df = 2)
1 - pchisq(lrt, df = 2)

