gf_point(distance ~ stretch, data = elasticband) %>%
  gf_lm() %>%
  gf_ribbon(lwr + upr ~ stretch, fill = "skyblue",
            data = cbind(elasticband, predict(eband.model, interval = "prediction"))) %>%
  gf_ribbon(lwr + upr ~ stretch, 
            data = cbind(elasticband, predict(eband.model, interval = "confidence"))) 
gf_point(distance ~ stretch, data = elasticband) %>%
  gf_lm(interval = "prediction", fill = "skyblue") %>% # prediction band the easy way
  gf_lm(interval = "confidence")     # confidence band the easy way

