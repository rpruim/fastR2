# P(B=b | W=w) = dhyper(b, 3, 5, 3 - w)
probs <- 
  outer(0:2, 0:3, function(x, y){dhyper(y, 3, 5, 3 - x)}) 
colnames(probs) = paste("B=", 0:3, sep="")
rownames(probs) = paste("W=", 0:2, sep="")
probs
fractions(probs)
#
# P(R=r | W=w) = dhyper(r, 5, 3, 3-w)
probs <- 
  outer(0:2, 0:3, function(x, y){dhyper(y, 5, 3, 3 - x)})
colnames(probs) = paste("R=", 0:3, sep = "")
rownames(probs) = paste("W=", 0:2, sep = "")
probs
fractions(probs)


