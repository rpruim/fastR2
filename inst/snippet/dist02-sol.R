require(MASS)                         # for fractions()
kernel <- function(x) { x * (x-3) * as.numeric(x >= 0 & x <= 3) }
k <- 1 / integrate(kernel, 0, 3)$value; k
fractions(k)
g <- function(x) { k * kernel(x) }
integrate(g, 0, 3)                    # check that we have pdf
integrate(g, 0, 1)
integrate(g, 0, 1) %>% value() %>% fractions()
integrate(g, 0, 2)
integrate(g, 0, 2) %>% value() %>% fractions()
integrate(g, 1, 2)
integrate(g, 1, 2) %>% value() %>% fractions()

