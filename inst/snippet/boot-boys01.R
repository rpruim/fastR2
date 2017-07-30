# select the 10 year old boys
Boys10 <- 
  NHANES::NHANES %>%
  filter(Age == 10, Gender == "male") %>%
  select(Age, Gender, Height) 

favstats( ~ Height, data = Boys10)

