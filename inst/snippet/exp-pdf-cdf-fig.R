plot_data <- 
  expand.grid(
    x = seq(0, 5, by = 0.01),
    lambda = c(0.5, 1, 1.5)
  ) %>% 
  mutate(
    density = dexp(x, rate = lambda),
    probability = pexp(x, rate = lambda)
  )
gf_line(density ~ x, color = ~ factor(lambda), group = ~lambda, data = plot_data) %>%
  gf_refine(guides(color = guide_legend(expression(lambda)))) %>%
  gf_labs(title = "Exponential pdfs")

gf_line(probability ~ x, color = ~ factor(lambda), group = ~lambda, data = plot_data) %>%
  gf_refine(guides(color = guide_legend(expression(lambda)))) %>%
  gf_labs(title = "Exponential cdfs")

