# any logical can be used to create subsets
data(faithful)
faithfulLong <- 
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting) %>%
  filter(duration > 3) 
xyplot( time_til_next ~ duration, faithfulLong )

