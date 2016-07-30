require(DAAG) 
eband.model <- lm(distance ~ stretch, data = elasticband); coef(eband.model)
xyplot(distance ~ stretch, data = elasticband, type = c('p', 'r'))

