ChickWeight %>% 
  filter(Time == 21) %>%
  arrange(weight) %>%
  head(1)
ChickWeight %>% 
  filter(Time == 21) %>%
  arrange(weight) %>%
  tail(1)

