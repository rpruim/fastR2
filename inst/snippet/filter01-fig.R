# any logical can be used to create subsets
data(faithful)
faithful2 <-
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting)
faithfulLong <- 
  faithful2 %>%
  filter(duration > 3) 
gf_point(time_til_next ~ duration, data = faithfulLong)

