data <- rnorm(40000, 10, 2.5)
histogram(~data, n = 10, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE))
histogram(~data, n = 40, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE), 
	lattice.options = list( 
		plot.polygon = list(border = trellis.par.get('plot.polygon')$col)
	)
)
histogram(~data, n = 160, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE),
	lattice.options = list( 
		plot.polygon = list(border = trellis.par.get('plot.polygon')$col)
	)
)

