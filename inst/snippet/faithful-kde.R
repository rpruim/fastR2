# times <- faithful$eruptions
# densityplot( ~ times, kernel = "rectangular",
#     main = "Rectangular kernel")
# densityplot( ~ times, kernel = "triangular",
#     main = "Triangular kernel")
# densityplot( ~ times,
#     main = "Normal kernel")
# densityplot( ~ times, adjust = 0.25,
#     main = "Normal kernel; adjust=0.25")
# density(times)       # display some information about the kde

plotFun(K1(x) ~ x, xlim = c(-3, 3), main = expression(K[1])) 
plotFun(K2(x) ~ x, xlim = c(-3, 3), main = expression(K[2]),
        discontinuity = Inf)
plotFun(K3(x) ~ x, xlim = c(-3, 3), main = expression(K[3]),
        discontinuity = Inf)
plotFun(K4(x) ~ x, xlim = c(-3, 3), main = expression(K[4]),
        discontinuity = Inf)

