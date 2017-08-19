Utilities3 <- Utilities3 %>% 
  mutate(monthShifted  = (month - 2) %% 12)
ut.lm4 <- 
  lm(thermsPerDay ~ monthShifted + I(monthShifted^2), data = Utilities3)
msummary(ut.lm4)
fit4 <- makeFun(ut.lm4)
gf_point(thermsPerDay ~ monthShifted, data = Utilities3) %>%
  gf_function(fit4, color = "red", alpha = 0.6)
ut.lm4 %>% Effect("monthShifted", ., partial.residuals = TRUE) %>% 
  plot("monthShifted")
plot(ut.lm4, w = 1:2)

