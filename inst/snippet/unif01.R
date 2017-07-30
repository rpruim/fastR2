x <- -1:11; x
f <- function(x) { 0.1 * (0 <= x & x <= 10) }
rbind(x, f(x))   # sanity check 
# numerical integration gives approximation and error estimate
integrate(f, 7, 10)   
integrate(f, 3, 7)
integrate(f, 7, 15)

