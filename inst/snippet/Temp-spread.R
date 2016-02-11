require(tidyr)
Temp2 <- 
  Temp %>%
  spread(key = month, value = temp)
Temp2

