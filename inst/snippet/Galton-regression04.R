GaltonBoys <-
  GaltonBoys %>%
  mutate(midparent = (father + mother) / 2)
favstats( ~ height, data = GaltonBoys)
favstats( ~ midparent, data = GaltonBoys)

