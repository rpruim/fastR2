p <- ppoints(10000)
x <- qnorm(p, 10, 2.5) # x <- rnorm(40000, 10, 2.5)
gf_dhistogram( ~ x, bins = 20, color = "black", fill = "navy", alpha = 0.5) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
gf_dhistogram( ~ x, bins = 40, color = "black", fill = "navy", alpha = 0.5) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
gf_dhistogram( ~ x, bins = 161, color = "black", fill = "navy", alpha = 0.5) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

