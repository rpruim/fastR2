times <- faithful$eruptions
histogram( ~ times, type = "density", breaks = seq(1, 6, by = 0.5))
densityplot( ~ times)

