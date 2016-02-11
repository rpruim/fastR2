vals <- 1:4
probs <- (4:1)/10
sum(vals*probs)                         # expected value
sum(vals^2*probs) - sum(vals*probs)^2   # variance 

