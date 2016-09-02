x <- rnorm(100)
histogram(~x, type = "density",
            panel = function(x, y, ...) {
                panel.rug(x, ...)
                panel.histogram(x, ...)
                panel.mathdensity(
                    dmath = dnorm, args = list(mean = mean(x), sd = sd(x)),
                    lwd = 5, col = "black", lty = 1, alpha = 0.5,
                    ...)
            }
     )
x <- rnorm(100)
histogram(~x, type = "density",
            panel = function(x, y, ...) {
                panel.rug(x, ...)
                panel.histogram(x, ...)
                panel.mathdensity(
                    dmath = dnorm, args = list(mean = mean(x), sd = sd(x)),
                    lwd = 5, col = "black", lty = 1, alpha = 0.5,
                    ...)
                grid::grid.text("Look Here",
                    x = 0.5, y = 0.48,
                    just = "left",
                    default.units = "npc",
                    rot = 33,
                    gp = grid::gpar(col = "black", cex = 2)
                    )
                grid::grid.segments( 
                    x0 = 0.48, x1 = unit(-0.92, "native"),
                    y0 = 0.45, y1 = unit(0.085, "native"),
                    arrow = grid::arrow(),           # default arrow
                    default.units = "npc",
                    gp = grid::gpar(col = "black")
                    )
                grid::grid.rect(x = -2, y = 0, width = 1, height = 0.15, 
                    default.units = "native",
                    just = c("left", "bottom"), 
                    gp = grid::gpar(
                      col = "black", fill = "gray40", alpha = 0.6)
                    )
            }
     )

