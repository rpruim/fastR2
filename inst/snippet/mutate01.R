data(Births78)
Births78 <- 
  mutate(Births78, runningTotal = cumsum(births))
head(Births78, 3)
gf_line(runningTotal ~ date, data = Births78)

