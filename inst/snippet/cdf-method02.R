fV <- function(v)  (0 <= v & v <= 4) * 0.25 / sqrt(abs(v)) 
integrate(fV, 0, 4)
# gf_fun is not clever about discontinuities
gf_fun(fV(v) ~ v, xlim = c(-1, 5), n = 1000) %>%
  gf_lims(y = c(0, 1))
# we can be clever if we do things manually
gf_line(y ~ v, data = data_frame(v = seq(-1, 5, by = 0.01), y = fV(v)),
        group =  ~(v < 0) + (v <=4)) %>%
  gf_lims(y = c(0, 1))

