GaltonBoys <- 
  GaltonBoys %>%
  mutate(zheight = zscore(height),
         zmidparent = zscore(midparent)
  )
Galtonz.lm <- lm(zheight ~ zmidparent, data = GaltonBoys) 
coef(Galtonz.lm)
xyplot(zheight ~ zmidparent, data = GaltonBoys, type = c("p", "r"))

