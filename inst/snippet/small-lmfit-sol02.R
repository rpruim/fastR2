#
# obtaining model fits by projection
#
p0 <- project(y, v0); p0
p1 <- project(y, v1); p1
p2 <- project(y, v2); p2
q1 <- project(y, w1); q1
q2 <- project(y, w2); q2
#
# this won't be a correct fit because dot(v1, v2) != 0
#
p0 + p1 + p2  

