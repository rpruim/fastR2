duration <- faithful$eruptions
gf_dens( ~ duration, kernel = "rectangular") %>% 
  gf_labs(title = "Rectangular kernel")
gf_dens( ~ duration, kernel = "triangular") %>% 
  gf_labs(title = "Triangular kernel")
gf_density( ~ duration) %>%
  gf_labs(title = "Normal kernel")
gf_density( ~ duration, adjust = 0.25) %>%
  gf_labs(title = "Normal kernel; adjust = 0.25")

density(duration)       # display some information about the kde

