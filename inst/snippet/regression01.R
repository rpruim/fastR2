GaltonBoys <- 
  Galton %>%
  filter(sex == "M") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup()
Galton.lm <- lm(height ~ father, data = GaltonBoys) 
coef(Galton.lm)
gf_lm(height ~ father, data = GaltonBoys) %>%
  gf_jitter(alpha = 0.4)

