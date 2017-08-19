data(elasticband, package = "DAAG") 
eband.model <- lm(distance ~ stretch, data = elasticband) 
coef(eband.model)
gf_lm(distance ~ stretch, data = elasticband) %>%
  gf_point()

