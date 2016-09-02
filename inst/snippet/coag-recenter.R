data(coagulation, package = "faraway")
Coag0 <-
  coagulation %>%
  mutate(grand.mean = mean(coag)) %>%
  group_by(diet) %>% 
  mutate(coag0 = coag - grand.mean) %>%
  ungroup()
lm(coag0 ~ -1 + diet, data = Coag0) %>%
  msummary()

