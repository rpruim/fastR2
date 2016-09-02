# remove first few observations because of bad meter read
Ut3 <- Utilities %>% filter(year > 2000 | month > 6)
ut.lm3 <- lm(thermsPerDay ~ month + I(month^2), data = Ut3)
msummary(ut.lm3)
plotModel(ut.lm3)
ut.lm3 %>% Effect("month", ., partial.residuals = TRUE) %>% plot("month")
plot(ut.lm3, w = 1:2)

