# partial regression plots (a.k.a. added-variable plots)
xyplot(resid(punting.lmy2) ~ resid(punting.lm12), type = c("p", "r"))
xyplot(resid(punting.lmy1) ~ resid(punting.lm21), type = c("p", "r"))

