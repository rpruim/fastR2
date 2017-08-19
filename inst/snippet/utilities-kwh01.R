Utilities2 <- 
  Utilities %>% 
  filter(year > 2000 | month > 6) %>%  # remove bad meter reading
  filter(temp <= 60) %>%               # remove warm months 
  mutate(kwhpday = kwh / billingDays)

