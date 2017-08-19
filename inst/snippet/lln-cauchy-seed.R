set.seed(123)
Data <- data_frame(
  n = 1:10000,
  x =  rcauchy(10000),
  running_mean =  cumsum(x) / (1:length(x))
)
gf_line(running_mean ~ n, data = Data) %>%
  gf_labs(y = "running mean", title = "Sample from a Cauchy Distribution")

