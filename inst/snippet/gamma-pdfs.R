h <- dgamma(0, 1, 1) * 1.05
dtemp <- function(x, mean = 0, sd = 1, rate = 1, shape = 1) {
        dgamma(x, shape = shape, rate = rate)
        }
xyplot(c(0, h) ~ c(0, 4), 
	main = "Gamma pdfs (rate=1)",
	xlim = c(0, 4),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			paste("shape = ", c(1, 2, 3), sep = ""),
			col = "gray30"
		),
		lines = list(
			col = trellis.par.get('superpose.line')$col[1:3],
			lty = 1,
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.95, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dmath = dtemp, args = list(shape = 1, rate = 1), n = 100,
			col = trellis.par.get('superpose.line')$col[1], lwd = 2, lty = 1)
		panel.mathdensity(dmath = dtemp, args = list(shape = 2, rate = 1), n = 100,
			col = trellis.par.get('superpose.line')$col[2], lwd = 2, lty = 1)
		panel.mathdensity(dmath = dtemp, args = list(shape = 3, rate = 1), n = 100,
			col = trellis.par.get('superpose.line')$col[3], lwd = 2, lty = 1)
	}
	)
xyplot(c(0, h) ~ c(0, 4), 
	main = "Gamma pdfs (shape=2)",
	xlim = c(0, 4),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			paste("rate=", c(0.5, 1.0, 2.0), sep = ""),
			col = "gray30"
		),
		lines = list(
			col = trellis.par.get('superpose.line')$col[c(2, 1, 3)],
			lty = 1,
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.95, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dmath = dtemp, args = list(shape = 2, rate = 0.5), n = 100,
			col = trellis.par.get('superpose.line')$col[2], lwd = 2, lty = 1)
		panel.mathdensity(dmath = dtemp, args = list(shape = 2, rate = 1), n = 100,
			col = trellis.par.get('superpose.line')$col[1], lwd = 2, lty = 1)
		panel.mathdensity(dmath = dtemp, args = list(shape = 2, rate = 2), n = 100,
			col = trellis.par.get('superpose.line')$col[3], lwd = 2, lty = 1)
	}
	)

