#
# now let's get the coefficients correctly:
#
b1 <- (q1/w1); b1   
b2 <- (q2/w2); b2  
a0 <- (p0/v0); a0
b0 <- a0 - b1*mean(x1) - b2*mean(x2); b0
coef(model)

