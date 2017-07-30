a <- rnorm(4000, mean = 10, sd = 2)
b <- rnorm(4000, mean = 10, sd = 5)
mydata <- data.frame(x = c(a, b), dist = rep(c("A", "B"), each = 4000))
gf_dhistogram( ~ x | dist, data = mydata,  bins = 30) %>% 
  gf_labs(x = "") 

