GaltonBoys <- 
  GaltonBoys %>%
  mutate(zheight = zscore(height),
         zmidparent = zscore(midparent)
  )
Galtonz.lm <- lm(zheight ~ zmidparent, data = GaltonBoys) 
coef(Galtonz.lm)
gf_lm(zheight ~ zmidparent, data = GaltonBoys) %>%
  gf_jitter(alpha = 0.4) %>%
  gf_abline(slope = 1, intercept = 0, alpha = 0.4)

