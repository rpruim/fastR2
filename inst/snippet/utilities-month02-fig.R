Ut3 <- Ut3 %>% 
  mutate(monthShifted  = (month - 2) %% 12)
ut.lm4 <- lm(thermsPerDay ~ monthShifted + I(monthShifted^2), data = Ut3)
msummary(ut.lm4)
plotModel(ut.lm4)
ut.lm4 %>% Effect("monthShifted", ., partial.residuals = TRUE) %>% 
  plot("monthShifted")
plot(ut.lm4, w = 1:2)

