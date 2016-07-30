# partial residual plot
xyplot(resid(ut.lm1) ~ kwhpday, data = Ut, type=c('p','r'))

