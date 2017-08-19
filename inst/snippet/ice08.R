Ice3 <- 
  Ice2 %>% filter(time == 1930) %>%
  spread(location, temp)
anova(lm(surface - intramuscular ~ treatment, 
         data = Ice3 %>% filter(phase == "t")))

