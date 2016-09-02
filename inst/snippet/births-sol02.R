Births %>% 
  group_by(month) %>%
  summarise(bpd = mean(births)) %>% 
  arrange(-bpd) %>% head(3)
Births %>% 
  group_by(month) %>%
  summarise(bpd = mean(births)) %>% 
  arrange(bpd) %>% head(3)

