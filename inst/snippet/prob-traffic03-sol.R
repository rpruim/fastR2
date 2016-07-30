xf <- function(x) { x * f(x) }
Ex <- integrate(xf, a, Inf)$value;  Ex       # E(X)
xxf <- function(x) { x^2 * f(x) }
Exx <- integrate(xxf, a, Inf)$value;  Exx    # E(X^2)
Exx - (Ex)^2                                 # variance
sqrt(Exx - (Ex)^2)                           # st dev

