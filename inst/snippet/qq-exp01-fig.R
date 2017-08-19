life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
mean(life01); 1 / mean(life01)
gf_qq( ~ life01, distribution = qexp, 
       dparams = list(1/mean(life01))) %>%
  gf_qqline(distribution = qexp, dparams = list(1/mean(life01)))

gf_qq( ~ life01, distribution = qexp) %>%
  gf_qqline(distribution = qexp)

