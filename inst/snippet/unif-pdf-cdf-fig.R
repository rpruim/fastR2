
gf_fun(dunif(x) ~ x, xlim = c(-0.5, 1.5), n = 1000) %>%
  gf_labs(title = "pdf for Unif(0,1)",
          x = "x", y = expression(f(x)))

gf_fun(punif(x) ~ x, xlim = c(-0.5, 1.5)) %>%
  gf_labs(title = "cdf for Unif(0,1)",
          x = "x", y = expression(F(x)))

