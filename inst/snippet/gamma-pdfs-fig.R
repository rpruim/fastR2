plot_data <- 
  expand.grid(
    x = seq(0, 6, by = 0.01),
    shape = 1:3,
    rate = 1:3
  ) %>% 
  mutate(
    density = dgamma(x, shape, rate),
    shape_label = paste("shape =", shape),
    rate_label = paste("rate =", rate)
    )

gf_line(density ~ x, data = plot_data %>% filter(rate == 2), 
        color = ~ factor(shape), group = ~shape) %>%
  gf_refine(guides(color = guide_legend("shape"))) %>%
  gf_labs(title = "Gamma pdfs (rate = 2)")

gf_line(density ~ x, data = plot_data %>% filter(shape == 2), 
        color = ~ factor(rate), group = ~rate) %>%
  gf_refine(guides(color = guide_legend("rate"))) %>%
  gf_labs(title = "Gamma pdfs (shape = 2)")

# gf_line(density ~ x | shape_label ~ rate_label, data = plot_data)

