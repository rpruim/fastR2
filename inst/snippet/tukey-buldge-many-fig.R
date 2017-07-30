gf_point(Y ~ X, data = ddd, color = ~original) %>%
  gf_facet_grid(paste("b=", b, sep = "") ~ paste("a=", a, sep = ""), scale = "free") %>%
  gf_theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none")

