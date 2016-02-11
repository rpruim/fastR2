## # this will be wrong for values not among 0, 1, 2, 3, or 4
## f <- function(x)  factorial(4) / (16 * factorial(x) * factorial(4-x))
## f(0:4)
## sum(f(0:4))    # check to be sure the probabilities add to 1
## xyplot(f(0:4)~0:4, xlab="x", ylab="probability")
## xyplot(f(0:4)~0:4, xlab="x", ylab="probability", type="h")
## xyplot(f(0:4)~0:4, xlab="x", ylab="probability", type=c("l","p"))

