h <- 4
dtemp <- function(x, mean = 0, sd = 1, shape1 = 1, shape2 = 1) {
  dbeta(x, shape1, shape2)
}

mylty <- c(1, 6, 6, 2, 2)
mycol <- trellis.par.get("superpose.line")$col[c(1, 2, 3, 2, 3)]
xyplot(c(0, h) ~ c(0, 1), 
	main = "Beta pdfs",
	xlim = c(0, 1),
	ylim = c(0, 5),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			paste0(  "shape1=", c(0.5, 2.0, 5.0, 2.0, 0.5), 
				   ", shape2=", c(0.5, 5.0, 2.0, 0.5, 2.0)),
			col = "gray30",
			cex = 0.7
		),
		lines = list(
			col = mycol,
			lty = mylty,
			lwd = c(2, 2, 2, 2, 2)
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(0.5, 1),
		x = 0.5, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dmath = dtemp, args = list(shape1 = 0.5, shape2 = 0.5), n = 100,
			col = mycol[1], lwd = 2, lty = mylty[1])
		panel.mathdensity(dmath = dtemp, args = list(shape1 = 2, shape2 = 5), n = 100,
			col = mycol[2], lwd = 2, lty = mylty[2])
		panel.mathdensity(dmath = dtemp, args = list(shape1 = 5, shape2 = 2), n = 100,
			col = mycol[3], lwd = 2, lty = mylty[3])
		panel.mathdensity(dmath = dtemp, args = list(shape1 = 2, shape2 = 0.5), n = 100,
			col = mycol[4], lwd = 2, lty = mylty[4])
		panel.mathdensity(dmath = dtemp, args = list(shape1 = 0.5, shape2 = 2), n = 100,
			col = mycol[5], lwd = 2, lty = mylty[5])
	}
	)

