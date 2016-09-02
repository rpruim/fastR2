model1 <- lm(distance ~ stretch, data = elasticband)
model2 <- lm(distance ~ stretch, data = RubberBand)
msummary(model1)
msummary(model2)

