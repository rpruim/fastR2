xyplot( ccf ~ (year + month/12), data = utilities, groups = month )
bwplot( ccf ~ factor(month), data = utilities )

