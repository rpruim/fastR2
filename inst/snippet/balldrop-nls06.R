balldrop.lm <- lm( log(time) ~ log(height), data = BallDrop)
balldrop.lm  %>% coef()
balldrop.lm  %>% coef() %>% getElement(1) %>% exp()
balldrop.nls %>% coef()
g <- makeFun(balldrop.lm)
gf_point(time ~ height, data = BallDrop) %>%
  gf_fun(f(height) ~ height, alpha = 0.4, size = 0.8) %>% 
  gf_fun(g(height) ~ height, col = "red", 
         linetype = 2, alpha = 0.7, size = 0.8)

