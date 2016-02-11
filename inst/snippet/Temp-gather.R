Temp3 <- 
  Temp2 %>%
  gather(key = month, value = temp, `1` : `12`)  
Temp3 %>% head

