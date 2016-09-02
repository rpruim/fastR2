Births %>% 
  group_by(wday) %>%
  summarise(bpd = mean(births)) %>% 
  arrange(-bpd) 

