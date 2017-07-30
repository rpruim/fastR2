cooling.model1 <- 
  nls(temp ~ A * exp( -k * time), data = CoolingWater1, 
      start = list(A = 100, k = 0.01))
f1 <- makeFun(cooling.model1)
gf_point(temp ~ time, data = CoolingWater1, size = 0.6) %>%
  gf_fun(f1(time) ~ time, lty = 2, col = "gray60") %>%
  gf_labs(y = "temp (C)", x = "time (sec)")

