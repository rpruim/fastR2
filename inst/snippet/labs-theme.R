gf_dens( ~ cesd, color = ~ substance, size = 1.5, data = HELPrct) %>%
  gf_labs(
    title = "Center for Epidemiologic Studies Depression measure",
    subtitle = "(at baseline)",
    color = "Abused substance: ",
    x = "CESD score",
    y = "",
    caption = "Source: HELPrct"
  ) %>%
  gf_theme(theme_classic()) %>%
  gf_theme(
    axis.text.y = element_blank(),
    legend.position = "top", 
    plot.title = element_text(hjust = 0.5, color = "navy"),
    plot.subtitle = element_text(hjust = 0.5, color = "navy", size = 12))

