HELPrct %>% 
  select(ends_with("e")) %>% 
  head(2)
HELPrct %>% 
  select(starts_with("h")) %>% 
  head(2)
HELPrct %>% 
  select(matches("i[12]")) %>% # regex matching
  head(2)  

