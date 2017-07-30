set.seed(1234) 
Batters2015 <- 
  Lahman::Batting %>% 
  filter(yearID == 2015) %>% 
  group_by(playerID) %>% 
  summarise(
    G=sum(G, na.rm = TRUE), 
    AB = sum(AB, na.rm = TRUE), 
    H = sum(H, na.rm = TRUE)) %>% 
  filter(AB >= 200) %>% 
  mutate(BA = round(H/AB, 3)) 
SampleBatters <- 
  Batters2015 %>%
  sample_n(16)
cat(paste(format(SampleBatters$BA[1:8], digits = 3), collapse = " & "))
cat("\\\\")
cat(paste(format(SampleBatters$BA[9:16], digits = 3), collapse = " & "))

