# standard error
12 / sqrt(36)
# usual standard error estimates
sapply(S, sd) / sqrt(36)
# bootstrap standard errors
sd( ~ boot.mean | sample, data = Boots)

