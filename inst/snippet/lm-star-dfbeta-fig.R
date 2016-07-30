xyplot(dfbeta(star.model2)[, 'temp'] ~ index, 
    data = HotStars,
    ylab = "DFBETA",
    panel = function(x, y, ...) {
        ids <- which(abs(y) > 0.5)
        panel.xyplot(x, y, ...)
        grid.text(
            x = x[ids] + 1.5, y = y[ids],
            as.character(ids), default.units = "native")
    })
coef(lm(light ~ temp, HotStars))
coef(lm(light ~ temp, HotStars[-7, ]))

