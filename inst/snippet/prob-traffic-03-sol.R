Ex <- integrate(xf, 1, Inf)$value;  Ex       # E(X)
xxf <- function(x) { x^2 * f(x) }
Exx <- integrate(xxf, 1, Inf)$value;  Exx    # E(X^2)
Exx - (Ex)^2                                 # variance
sqrt(Exx - (Ex)^2)                           # st dev

