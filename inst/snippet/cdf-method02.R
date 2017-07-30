fV <- function(v)  (0 <= v & v <= 4) * 0.25 / sqrt(abs(v)) 
integrate(fV, 0, 4)
gf_fun(fV(v) ~ v, xlim = c(-1, 5), n = 1000) %>%
  gf_lims(y = c(0, 1))

