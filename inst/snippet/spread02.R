require(tidyr) 
Ut3 <- 
  Ut2 %>%
  spread(key = month, value = temp)
Ut3 %>% head(4)

