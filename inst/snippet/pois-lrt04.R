ml.pois100 <-
  maxLik2(loglik.pois, start = c(lambda = 1), x.bar = 1.4, n = 100) 
plot(ml.pois100) %>% gf_labs(title = "n = 100")

