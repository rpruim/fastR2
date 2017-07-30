sim3 <- 
  function(
    b0 = 3, b1 = 5, lambda = 1, 
    x = rep(1:5, each = 4)    # 4 observations at each of 5 values
    ){
    e <- x * rnorm(length(x))
    y <- b0 + b1 * x + e
    model <- lm(y ~ x)  
    ci <- confint(model, 2)
    dimnames(ci)[[2]] <- c("lo", "hi")   # provide nicer names
    ci
  }
Sims3 <- do(5000) * sim3()
Sims3 <-
  Sims3 %>% 
  mutate(status = ifelse(lo > 5, "hi", ifelse(hi < 5, "lo", "good"))) 

