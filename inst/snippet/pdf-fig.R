# define the pdf for X
f <- function(x) { x^2 / 9  * (0 <= x & x <= 3) }
# numerical integration gives approximation and tolerance
integrate(f, 0, 3)
integrate(f, -Inf, Inf)              # same value but less precise
integrate(f, 0, 1)
integrate(f, 0, 1) %>% value()       # just the approximation value
# find nearby fraction
integrate(f, 0, 1) %>% value() %>% fractions() 
gf_line(y ~ x, data = data_frame(x = seq(-1, 4, by = 0.01), y = f(x)),
        group = ~ (x > 3)) %>%
  gf_labs(y = "f(x)")

