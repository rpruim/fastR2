f <- function(x, k=0) {x^k * x/2 }         # define pdf
integrate(f, lower = 0, upper = 2)         # check it is a pdf
integrate(f, k=1, lower = 0, upper = 2)    # expected value
integrate(f, k=2, lower = 0, upper = 2)    # E(X^2)

