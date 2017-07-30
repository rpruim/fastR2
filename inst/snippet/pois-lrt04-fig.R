# We can express l() in terms of sufficient statistics 
loglik.pois <- function(theta, x.bar = 1.4, n = 10) {
  - n * theta + n * x.bar * log(theta)
}
ml.pois10 <- 
  maxLik2(loglik.pois, start = c(lambda = 1), x.bar = 1.4, n = 10)
plot(ml.pois10) %>% gf_labs(title = "n = 10")
ml.pois100 <-
  maxLik2(loglik.pois, start = c(lambda = 1), x.bar = 1.4, n = 100) 
plot(ml.pois100) %>% gf_labs(title = "n = 100")

