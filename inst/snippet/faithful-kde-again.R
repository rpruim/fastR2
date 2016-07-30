times <- faithful$eruptions
densityplot( ~ times, kernel = "rectangular",
    main = "Rectangular kernel")
densityplot( ~ times, kernel = "triangular",
    main = "Triangular kernel")
densityplot( ~ times,
    main = "Normal kernel")
densityplot( ~ times, adjust = 0.25,
    main = "Normal kernel; adjust=0.25")
#density(times)       # display some information about the kde

