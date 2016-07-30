# modify data by dropping first observation
concretemod <- concrete[-1,]
concrete.lmmod <- lm(strength ~ limestone + water, data = concretemod)
coef(concrete.lmmod)
y <- concretemod$strength
n <- length(y); v0 <- rep(1,n)
v1 <- with(concretemod, limestone - mean(limestone))
v2 <- with(concretemod, water - mean(water))
project(y,v0,type='v')
mean(y)
dot(y, v1) / vlength(v1)^2
dot(y, v2) / vlength(v2)^2
ef0 <- project(y, v0)
ef1 <- project(y, v1)
ef2 <- project(y, v2)
ef0 + ef1 + ef2
fitted(concrete.lmmod)

