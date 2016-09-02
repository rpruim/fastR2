GaltonBoys <- 
  GaltonBoys %>%
  mutate(zheight = zscore(height),
         zmidparent = zscore(midparent)
  )
Galtonz.lm <- lm(zheight ~ zmidparent, data = GaltonBoys) 
coef(Galtonz.lm)
xyplot(zheight ~ zmidparent, data = GaltonBoys, type = c("p", "r"),
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(0, 1, lty = 2, col = "gray70")
       }
)

