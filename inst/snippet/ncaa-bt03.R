# look at top teams  
BTabilities(NCAA.model) %>% 
  as.data.frame() %>% 
  mutate(team = row.names(BTabilities(NCAA.model))) %>% 
  arrange(-ability) %>% head(6)

