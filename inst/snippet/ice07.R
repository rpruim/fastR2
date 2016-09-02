Ice2 <-
  Ice %>% 
  select(subject, treatment, location, t1930) %>% 
  tidyr::spread(location, t1930) %>% 
  rename(surfTemp = surface, intraTemp = intramuscular)
anova(lm(surfTemp - intraTemp ~ treatment, data = Ice2))

