gf_hex(sigma ~ mu, data = PosteriorSample, 
       bins = 50) %>%
  gf_density2d(color = "white", alpha = 0.5) %>% 
  gf_refine(scale_fill_gradient2(midpoint = 400, low = "gray80", mid = "skyblue", high = "navy")) %>%
  gf_theme(legend.position = "top")

