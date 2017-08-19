y <- Concrete$strength
n <- length(y); v0 <- rep(1, n)
v1 <- with(Concrete, limestone - mean(limestone))
v2 <- with(Concrete, water - mean(water))
dot(y, v0) / vlength(v0)^2
mean(y)
dot(y, v1) / vlength(v1)^2
dot(y, v2) / vlength(v2)^2

