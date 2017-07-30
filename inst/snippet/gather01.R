Ut4 <- 
  Ut3 %>%
  gather(key = month, value = temp, `1` : `12`)  
Ut4 %>% head(4)

