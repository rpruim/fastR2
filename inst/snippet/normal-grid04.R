gf_point(sigma ~ mu, data = PosteriorSample, 
         size = 0.4, alpha = 0.15) %>%
  gf_density2d()

