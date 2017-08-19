t <- (10.3 - 10)/ (0.4 / sqrt(12)); t    # test statistic
2 * pt(-abs(t), df = 11);        # p-value using t-distribution
2 * pnorm(-abs(t));              # "p-value" using normal distribution

