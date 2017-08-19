gf_dist("binom", params = list(size = 20, prob = 0.1)) %>%
  gf_dist("norm", params = list(mean = 2, sd = sqrt(0.1 * 0.9 * 20)), 
          alpha = 0.4) %>%
  gf_labs(title = expression(paste("Binomial vs Normal", "(", "n=20", ", " , pi, "= 0.10", ")")))

