plot_data <- 
  expand.grid(
    x = seq(0, 1, by = 0.01),
    shape1 = c(0.8, 2, 4),
    shape2 = c(0.8, 2, 4)
  ) %>% 
  mutate(
    density = dbeta(x, shape1 = shape1, shape2 = shape2),
    shape1_label = paste("shape1 =", shape1),
    shape2_label = paste("shape2 =", shape2)
    )

# gf_line(density ~ x, data = plot_data %>% filter(shape2 == 2),
#         color = ~ factor(shape1), group = ~shape1) %>%
#   gf_refine(guides(color = guide_legend("shape1"))) %>%
#   gf_labs(title = "Beta pdfs (shape2 = 2)")
# 
# gf_line(density ~ x, data = plot_data %>% filter(shape1 == 2),
#         color = ~ factor(shape2), group = ~shape2) %>%
#   gf_refine(guides(color = guide_legend("shape2"))) %>%
#   gf_labs(title = "Beta pdfs (shape1 = 2)")

gf_line(density ~ x | shape2_label ~ shape1_label, data = plot_data)

