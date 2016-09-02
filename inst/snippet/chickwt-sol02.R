Chicks <- 
  ChickWeight %>% 
  filter(Time > 15) %>%   # remove chicks that were only measured a few times
  group_by(Chick) %>%
  summarise(
    weight = max(weight),
    diet = Diet[1],
    time = max(Time)
    ) %>%
  ungroup() %>%     # need this for arrange to work properly
  arrange(weight) 
Chicks %>% head(1)
Chicks %>% tail(1)


