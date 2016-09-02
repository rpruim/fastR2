observed <- tbl2[ , 3:4]
expected <- tbl2[ , 6:7]
lrt <- 2 * sum(observed *  log (observed / expected)); lrt
pearson <- sum((observed - expected)^2 / expected); pearson

