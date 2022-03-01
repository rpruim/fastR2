time <- c(49.0, 60.4, 8.9, 43.4, 34.8, 8.2, 13.6, 11.5, 99.4, 31.9)  
mean(time)
lambda.hat = 1 / mean(time); lambda.hat
Plot_data <- tibble(x = seq(0,121, by = 0.5), density = dexp(x, rate = lambda.hat))

gf_dhistogram( ~ time, n = 10, binwidth = 10, alpha = 0.5) %>%
  gf_line(density ~ x, data = Plot_data)

breaks = seq(0, 11, by = 2)^2
gf_dhistogram( ~ time, n = 10, breaks = breaks, alpha = 0.5) %>%
  gf_line(density ~ x, data = Plot_data)

