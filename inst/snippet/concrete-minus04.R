y <- Concretemod$strength
# make fits using v1 and w2
ef0 <- project(y, v0)
ef1 <- project(y, v1)
ef2 <- project(y, w2)
ef0 + ef1 + ef2
# now try w1 and v2
ef0 <- project(y, v0)
ef1 <- project(y, w1)
ef2 <- project(y, v2)
ef0 + ef1 + ef2
# should match what lm() produces
fitted(concrete.lmmod)

