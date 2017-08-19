model <- lm(y ~ x , data = SmallData) 
gf_lm(y ~ x, data = SmallData) %>%
  gf_point()

