# remove first few observations because of bad meter read
Utilities3 <- 
  Utilities %>% filter(year > 2000 | month > 6)
ut.lm3 <- lm(thermsPerDay ~ month + I(month^2), data = Utilities3)
msummary(ut.lm3)
fit3 <- makeFun(ut.lm3)
gf_point(thermsPerDay ~ month, data = Utilities3) %>%
  gf_function(fit3, color = "red", alpha = 0.6)
ut.lm3 %>% Effect("month", ., partial.residuals = TRUE) %>% plot("month") 
plot(ut.lm3, w = 1:2)

