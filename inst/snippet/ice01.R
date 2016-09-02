Ice %>% 
  group_by(location, treatment) %>%
  summarise(mean(b1930))

