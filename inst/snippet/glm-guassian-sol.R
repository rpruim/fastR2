glm(stretch ~ distance, data = elasticband, 
    family = gaussian()) %>% 
  msummary()
lm(stretch ~ distance, data = elasticband) %>% msummary()
lm(stretch ~ distance, data = elasticband) %>% anova()


