plot_data <- 
  expand.grid(
    x = seq(-4, 4, by = 0.01),
    sigma = c(0.7, 1.0, 1.5)
  ) %>% 
  mutate(density = dnorm(x, 0, sigma))

gf_line(density ~ x, data = plot_data, color = ~ factor(sigma), group = ~sigma) %>%
  gf_refine(guides(color = guide_legend(expression(sigma))))

