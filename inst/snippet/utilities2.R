Utilities <- mutate(Utilities, ccfpday = ccf / billingDays)
xyplot( ccfpday ~ (year + month/12), data = Utilities, groups = month )
bwplot( ccfpday ~ factor(month), data = Utilities )

