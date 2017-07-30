rstudent(small.model)
# computing the first externally studentized residual by hand
smaller.model <- lm(y ~ x, data = SmallData[-1, ])
# modified sigma.hat based on smaller model
summary(smaller.model)$sigma
sigma.hat <- sqrt(sum(resid(smaller.model)^2) / 1); sigma.hat  
# rescaling the 1st residual
e[1] / (sigma.hat * sqrt(1 - h[1]))        

