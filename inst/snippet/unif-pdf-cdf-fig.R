
gf_line(y ~ x, 
        data = tibble(x = seq(-0.5, 1.5, by = 0.001), y = dunif(x)),
        group = ~ (x < 0) + (x<=1) ) %>%
  gf_labs(y = "f(x)")

# gf_fun(dunif(x) ~ x, xlim = c(-0.5, 1.5), n = 1000) %>%
#   gf_labs(title = "pdf for Unif(0,1)",
#           x = "x", y = expression(f(x)))

gf_fun(punif(x) ~ x, xlim = c(-0.5, 1.5)) %>%
  gf_labs(title = "cdf for Unif(0,1)",
          x = "x", y = "F(x)")

