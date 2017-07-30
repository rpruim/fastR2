sim <- 
  function(
    b0 = 3, b1 = 5, sigma = 2, 
    x = rep(1:5, each = 4)         # 4 observations at each of 5 values
    ){
    e <- rnorm(length(x), sd = sigma)
    y <- b0 + b1 * x + e
    model <- lm(y ~ x)  
    ci <- confint(model, 2)
    dimnames(ci)[[2]] <- c("lo", "hi")   # provide nicer names
    ci
  }

