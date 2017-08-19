glm(cbind(W, L) ~ SLG, data = BB, family = "binomial") %>%
  msummary()

