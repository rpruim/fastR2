# partial residual plot
xyplot(resid(ut.lm1) ~ kwhpday, ut, type=c('p','r'))

