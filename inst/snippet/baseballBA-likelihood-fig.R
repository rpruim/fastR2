dat <- expand.grid(
	alpha = seq(4, 210, by = 6),
	beta = seq(10, 500, by = 15)
	)

dat$loglik <- apply(cbind(dat$alpha, dat$beta), 1, FUN = "loglik", x = ba)

wireframe(
  exp(loglik) ~ alpha * beta, dat, 
  col = "gray25",
  par.settings = list(
    box.3d = list(col = "transparent"),
    axis.line = list(col = NA, lty = 1, lwd = 1)
  ),
  shade = FALSE, 
  light.source = c(25, 50, 50),
  aspect = c(1, 0.4),
  screen = list(z = 20, x = -75),
  xlab = expression(alpha),
  ylab = expression(beta),
  zlab = "",
  scale = list(arrows = FALSE, z = list(draw = FALSE))
)

dat <- expand.grid(
	alpha = seq(4, 325, by = 1),
	beta = seq(10, 800, by = 2)
	) 

dat$loglik <- apply(cbind(dat$alpha, dat$beta), 1, FUN = "loglik", x = ba)

levelplot(loglik ~ alpha + beta, data = dat,
          xlab = expression(alpha),
          ylab = expression(beta),
          main = "log-likelihood", 
          col.regions = topo.colors(n=100)
)

