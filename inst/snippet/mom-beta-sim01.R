Results <- do(1000) * beta.mom(rbeta(50, 2, 5))
histogram( ~ shape1, data = Results, type = "density", v = 2)
histogram( ~ shape2, data = Results, type = "density", v = 5)

