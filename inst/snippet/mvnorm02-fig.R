plotFun(f(x, y) ~ x + y, x.lim = c(-3, 3), y.lim = c(-3, 3), npts = 22,
        surface = TRUE,  
        interactive = FALSE,
        par.settings = list(
          box.3d = list(col = "transparent"),
          axis.line = list(col = NA, lty = 1, lwd = 1)
        ),
        light.source = c(25, 50, 50),
        aspect = c(1, 0.5),
        zlab = "",
        screen = list(z = 20, x = -75),
        scales = list(arrows = FALSE, 
                      x = list(draw = FALSE),
                      y = list(draw = FALSE),
                      z = list(draw = FALSE))
)
plotFun(g(x, y) ~ x + y, x.lim = c(-3, 3), y.lim = c(-3, 3), npts = 22,
        surface = TRUE, 
        interactive = FALSE,
        par.settings = list(
          box.3d = list(col = "transparent"),
          axis.line = list(col = NA, lty = 1, lwd = 1)
        ),
        light.source = c(25, 50, 50),
        aspect = c(1, 0.5),
        zlab = "",
        screen = list(z = 20, x = -75),
        scales = list(arrows = FALSE, 
                      x = list(draw = FALSE),
                      y = list(draw = FALSE),
                      z = list(draw = FALSE))
)

