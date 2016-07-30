xyplot(ccf ~ (year + month/12), data = Utilities, groups = month)
bwplot(ccf ~ factor(month), data = Utilities)

