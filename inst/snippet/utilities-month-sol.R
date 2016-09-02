ut.mod <-
  Ut3 %>% mutate(monthShifted2 = (month + 5) %% 12) %>%
  lm(thermsPerDay ~ poly(monthShifted2, 2), data = .) 
msummary(ut.mod)
plot(ut.mod, 1:2)
ut.mod %>% Effect("monthShifted2", ., partial.residuals = TRUE) %>%
  plot("monthShifted2")

