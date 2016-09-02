Births %>% 
  group_by(moth, day) %>%
  summarise(bpd = mean(births)) %>% 
  arrange(-bpd) %>% head(4)
Births %>% 
  group_by(month, day) %>%
  summarise(bpd = mean(births)) %>% 
  arrange(bpd) %>% head(4)

