x <- c(-0.16, 1.17, -0.43, -0.02, 1.06, 
       -1.35, 0.65, -1.12, 0.03, -1.44)
Plot_data <- data_frame(
  x.sorted = sort(x),
  p = seq(0.05, 0.95, by = 0.1),
  q = qnorm(p)
  )
Plot_data
gf_point(x.sorted ~ q, data = Plot_data)
gf_qq( ~ x)               # generate the normal-quantile plot

