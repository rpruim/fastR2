cooling.model2 <- 
  nls(temp ~ ambient + A * exp( - k * (1 + time)), 
      data = CoolingWater1,
      start = list(ambient = 20, A = 80, k = 0.01) )
f2 <- makeFun(cooling.model2)
gf_point(temp ~ time, data = CoolingWater1) %>%  
  gf_fun(f1(time) ~ time, lty = 2, col = "gray60") %>%
  gf_fun(f2(time) ~ time, col = "red", size = 0.8) %>%
  gf_labs(y = "temp (C)", x = "time (sec)")

