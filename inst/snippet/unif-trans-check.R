fV <- function(v)  (0 <= v & v <= 4) * 0.25 / sqrt(abs(v)) 
integrate(fV, 0, 4)
plotFun(fV(v) ~ v, v.lim = c(-5, 5))

