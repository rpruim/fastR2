p <- ppoints(10000)
x <- qnorm(p, 10, 2.5) # x <- rnorm(40000, 10, 2.5)
histogram( ~ x, n = 10, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE))
histogram( ~ x, n = 40, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE), 
	lattice.options = list( 
		plot.polygon = list(border = trellis.par.get("plot.polygon")$col)
	)
)
histogram( ~ x, n = 161, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE),
    par.settings = list(plot.polygon = list(border = "transparent")),
	lattice.options = list( 
		plot.polygon = list(border = trellis.par.get("plot.polygon")$col)
	)
)

