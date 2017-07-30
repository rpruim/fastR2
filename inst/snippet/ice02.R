Ice2 %>% filter(time == 1930, phase == "b") %>%
  group_by(location, treatment, phase) %>%
  summarise(mean(temp))

