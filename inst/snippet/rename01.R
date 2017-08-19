data(faithful)
faithful <- 
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting)
faithful %>% head(3)
gf_point(time_til_next ~ duration, data = faithful)

