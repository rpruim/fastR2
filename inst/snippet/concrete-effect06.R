lm(strength ~ limestone + water, data = Concrete) %>% 
  msummary()
lm(strength ~ limestone + water + limestone * water, data = Concrete) %>% 
  msummary()

