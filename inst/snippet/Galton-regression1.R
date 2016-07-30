GaltonBoys <- 
  Galton %>%
  filter(sex == "M") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup()
Galton.lm <- lm(height ~ father, data = GaltonBoys) 
coef(Galton.lm)
xyplot(height ~ father, data = GaltonBoys, type = c("p", "r"))
projectedHeight <- makeFun(Galton.lm) 

