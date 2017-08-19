plot_data <- 
  expand.grid(
    x = seq(0, 6, by = 0.01),
    shape = 1:3,
    scale = 1:3
  ) %>% 
  mutate(
    density = dweibull(x, shape = shape, scale = scale),
    shape_label = paste("shape =", shape),
    scale_label = paste("scale =", scale)
    )

gf_line(density ~ x, data = plot_data %>% filter(scale == 2), 
        color = ~ factor(shape), group = ~shape) %>%
  gf_refine(guides(color = guide_legend("shape"))) %>%
  gf_labs(title = "Weibull pdfs (scale = 2)")

gf_line(density ~ x, data = plot_data %>% filter(shape == 2), 
        color = ~ factor(scale), group = ~scale) %>%
  gf_refine(guides(color = guide_legend("scale"))) %>%
  gf_labs(title = "Weibull pdfs (shape = 2)")

# gf_line(density ~ x | shape_label ~ scale_label, data = plot_data)

