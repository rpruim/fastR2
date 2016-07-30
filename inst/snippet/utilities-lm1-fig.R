# subset the data:
#   * remove first few months where there appears to have been a bad
#         meter reading (year == 2000 & month <= 6)
#   * remove months where there is little need to heat (temp > 60)
Ut <- Utilities2 %>% filter((year > 2000 | month > 6) & temp <= 60)
ut.lm1 <- lm(thermsPerDay ~ temp, data = Ut)
summary(ut.lm1)
xplot(ut.lm1)

