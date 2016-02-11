data(Births78)
Births <- 
  mutate( 
    Births78, 
    dayofweek = lubridate::wday(date, label = TRUE, abbr = TRUE))
head(Births, 3)

