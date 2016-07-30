sim2 <- 
  function(
    b0 = 3, b1 = 5, lambda = 1,
    x = rep(1:5, each = 4)       # 4 observations at each of 5 values
    ){
    # shift to give a mean of 0.
    e <- rexp(length(x), rate = 1 / lambda) - lambda
    y <- b0 + b1 * x + e
    model <- lm(y ~ x)  
    ci <- confint(model, 2)
    dimnames(ci)[[2]] <- c("lo", "hi")   # provide nicer names
    ci
  }
Sims2 <- do(5000) * sim2()
Sims2 <-
  Sims2 %>% 
  mutate(status = ifelse(lo > 5, "hi", ifelse(hi < 5, "lo", "good"))) 
tally( ~ status, data = Sims2) / 5000

binom.test( ~ status, data = Sims2, p = 0.95)
chisq.test(tally( ~ status, data = Sims2), p = c(0.95, 0.025, 0.025))

