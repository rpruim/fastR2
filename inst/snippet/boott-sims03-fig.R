gf_linerange(lo + hi ~ .index, data = Sims,
             color = ~ status, size = 0.5, alpha = 0.7) %>%
  gf_point(estimate ~ .index, size = 0.5, alpha = 0.7) %>%
  gf_facet_grid(method ~ .) %>%
  gf_refine(scale_color_manual(
    values = c(good = "gray70", hi = "red", lo = "navy"),
    breaks = c("hi", "good", "lo")
    )) %>%
  gf_labs(x = "")

