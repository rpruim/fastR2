HELPrct %>% 
  group_by(sex, substance) %>%
  summarise(x.bar = mean(age), s = sd(age))

