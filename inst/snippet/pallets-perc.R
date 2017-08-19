Pallets2 <-
  Pallets %>% 
  group_by(day) %>% mutate(total = sum(pallets)) %>%
  group_by(day, employee) %>% mutate(perc = 100 * pallets / total)

