ml.binom <- maxLik2(loglik.binom, x = 35, n = 55, start = 0.5) 
plot(ml.binom, ci = c("w", "l"), hline = TRUE) %>%
  gf_labs(x = expression(pi))

