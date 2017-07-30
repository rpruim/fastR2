life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
gf_qq( ~ life01, distribution = qweibull, 
       dparams = list(shape = 1.4, scale = 144)) %>%
  gf_abline(slope = 1, intercept = 0, color = "gray50", 
            linetype = "dashed")

