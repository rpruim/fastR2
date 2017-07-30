sim()       # one simulation
Sims <- do(5000) * sim()   # lot of simulations
Sims <-
  Sims %>% 
  mutate(status = ifelse(lo > 5, "hi", ifelse(hi < 5, "lo", "good"))) 

