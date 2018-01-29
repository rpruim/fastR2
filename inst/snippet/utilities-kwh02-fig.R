# fit additive and interaction models
ut.lm    <- lm(thermsPerDay ~ temp + kwhpday, data = Utilities2)
ut.lmint <- lm(thermsPerDay ~ temp * kwhpday, data = Utilities2)
msummary(ut.lm)
msummary(ut.lmint)
plot(ut.lm, 1:2)
plot(ut.lmint, 1:2)
ut.lm %>% 
  Effect(c("temp", "kwhpday"), . , partial.residuals = TRUE) %>% 
  plot("temp", sub = "additive model")
ut.lmint %>% 
  Effect(c("temp", "kwhpday"), . , partial.residuals = TRUE) %>% 
  plot("temp", sub = "interation model", alternating = FALSE)

