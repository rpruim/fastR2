# define the pdf for X
f <- function(x) { x^2 / 9  * (0 <= x & x <= 3) }
# numerical integration gives approximation and tolerance
integrate(f, 0, 3)
integrate(f, -Inf, Inf)              # same value but less precise
integrate(f, 0, 1)
integrate(f, 0, 1) %>% value()       # just the approximation value
# find nearby fraction
integrate(f, 0, 1) %>% value() %>% fractions() 
plotFun(f(x) ~ x, xlim = c(-1, 4))

