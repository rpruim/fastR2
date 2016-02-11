faithful <-
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting)

