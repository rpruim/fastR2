# set up the v an u vectors
v0 <- rep(1,4); v0
u0 <- v0/vlength(v0); u0
v1 <- x - mean(x); v1
u1 <- v1/vlength(v1); u1
#
# projecting into the model space
project(y,v0)
project(y,v1)
project(y,v0) + project(y,v1)    # fitted values
fitted(model)
#
# two ways to compute beta_1-hat
b1 <- project(y,v1,type='l')/vlength(v1); b1
b1 <- dot(y,v1)/(vlength(v1))^2; b1
#
# two ways to compute alpha_0-hat
a0 <- project(y,v0,type='l')/vlength(v0); a0
a0 <- dot(y,v0)/(vlength(v0))^2; a0
#
# beta_0-hat
b0 <- a0 - b1 * mean(x); b0

