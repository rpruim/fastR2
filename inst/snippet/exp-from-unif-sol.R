U <- runif(10000) 
X <- (-10 * log(U)) %>% sort()
Y1 <- rexp(10000, rate = 1/10) %>% sort()
Y2 <- rexp(10000, rate = 1/10) %>% sort()
xyplot(Y1 ~ X, panel = function(x, y, ...){
  panel.abline(0, 1)
  panel.xyplot(x, y, ...)
  })
xyplot(Y2 ~ Y1, panel = function(x, y, ...){
  panel.abline(0, 1)
  panel.xyplot(x, y, ...)
  })

