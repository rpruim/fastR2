gf_dhistogram( ~ sigma^2, data = MLEs, binwidth = 5)
gf_qq( ~ sigma^2, data = MLEs, geom = "line", 
         distribution = qchisq, dparams = list(df = 29)) %>%
  gf_labs(x = "Chisq(29)", y = expression(hat(sigma)^2))

