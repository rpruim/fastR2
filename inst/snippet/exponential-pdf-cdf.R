h <- 1.5
xyplot(c(0, h) ~ c(0, 5), 
	main = "Exponential pdfs",
	xlab = "",
	ylab = "Density",
	scales = list(axs = "i"),
	key = list(
		text = list(
			c(expression(paste(lambda, "=1")),
				expression(paste(lambda, "=0.5")),
				expression(paste(lambda, "=1.5"))
			),
			col = "gray30", cex = 0.7
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[1:3],
			lty = 1, 
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.95, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dexp, args = list(rate = 1), n = 100, 
			col = trellis.par.get("superpose.line")$col[1], lwd = 2, lty = 1)
		panel.mathdensity(dexp, args = list(rate = 0.5), n = 100, 
			col = trellis.par.get("superpose.line")$col[2], lwd = 2, lty = 1)
		panel.mathdensity(dexp, args = list(rate = 1.5), n = 100, 
			col = trellis.par.get("superpose.line")$col[3], lwd = 2, lty = 1)
	}
	)

mypexp <- function(x, ...){ pexp(q = x, ...) }

xyplot(c(0, h) ~ c(0, 5), 
	main = "Exponential cdfs",
	xlab = "",
	ylab = "Probability",
	scales = list(axs = "r"),
	key = list(
		text = list(
			c(expression(paste(lambda, "=1")),
				expression(paste(lambda, "=0.5")),
				expression(paste(lambda, "=1.5"))
			),
			col = "gray30", cex = 0.7
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[1:3],
			lty = 1, 
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.95, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(mypexp, args = list(rate = 1), n = 100,
			col = trellis.par.get("superpose.line")$col[1], lwd = 2, lty = 1)
		panel.mathdensity(mypexp, args = list(rate = 0.5), n = 100,
			col = trellis.par.get("superpose.line")$col[2], lwd = 2, lty = 1)
		panel.mathdensity(mypexp, args = list(rate = 1.5), n = 100,
			col = trellis.par.get("superpose.line")$col[3], lwd = 2, lty = 1)
	}
	)

