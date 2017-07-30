times <- faithful$eruptions
gf_dhistogram( ~ times, binwidth = 0.25, alpha = 0.3) %>%
gf_dens( ~ times, size = 1, alpha = 0.9)

