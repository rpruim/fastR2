h <- 1.6
dtemp <- function(x, mean = 0, sd = 1, scale = 1, shape = 1) {
  dweibull(x, scale = scale, shape = shape)
}

xyplot(c(0, h) ~ c(0, 3), 
	main = "Weibull pdfs (shape=1)",
	xlim = c(0, 2.2),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			paste("scale = ", 1:3, sep = ""),
			col = "gray30",
			cex = 0.7
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[1:3],
			lty = 1, 
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.97, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dmath = dtemp, args = list(scale = 1, shape = 1),  n = 100,
			col = trellis.par.get("superpose.line")$col[1], lwd = 2, lty = 1) 
		panel.mathdensity(dmath = dtemp, args = list(scale = 2, shape = 1), n = 100,
			col = trellis.par.get("superpose.line")$col[2], lwd = 2, lty = 1) 
		panel.mathdensity(dmath = dtemp, args = list(scale = 3, shape = 1), n = 100,
			col = trellis.par.get("superpose.line")$col[3], lwd = 2, lty = 1) 
	}
	)

xyplot(c(0, h) ~ c(0, 3), 
	main = "Weibull pdfs (scale=1)",
	xlim = c(0, 2.2),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			paste("shape = ", 1:3, sep = ""),
			col = "gray30",
			cex = 0.7
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[1:3],
			lty = 1, 
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.97, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dmath = dtemp,  args = list(scale = 1, shape = 1),  n = 100,
			col = trellis.par.get("superpose.line")$col[1], lwd = 2) 
		panel.mathdensity(dmath = dtemp, args = list(scale = 1, shape = 2), n = 100,
			col = trellis.par.get("superpose.line")$col[2], lwd = 2) 
		panel.mathdensity(dmath = dtemp, args = list(scale = 1, shape = 0.5), n = 100,
			col = trellis.par.get("superpose.line")$col[3], lwd = 2) 
	}
	)

