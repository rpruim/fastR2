# modify data by dropping first observation
Concretemod <- Concrete[-1, ]
concrete.lmmod <- lm(strength ~ limestone + water, data = Concretemod)
coef(concrete.lmmod)
y <- Concretemod$strength
n <- length(y); v0 <- rep(1, n)
v1 <- with(Concretemod, limestone - mean(limestone))
v2 <- with(Concretemod, water - mean(water))
project(y, v0)
mean(y)
dot(y, v1) / vlength(v1)^2
dot(y, v2) / vlength(v2)^2
ef0 <- project(y, v0)
ef1 <- project(y, v1)
ef2 <- project(y, v2)
ef0 + ef1 + ef2
fitted(concrete.lmmod)

