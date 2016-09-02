rm <- seq(-5, 5, by = 0.1)
wp <- makeFun(glm.bb)(runmargin = rm)
xyplot(winP ~ runmargin, data = BB, xlim = c(-2.5, 2.5), ylim = c(0, 1),
    panel = function(x, y, ...){
        panel.xyplot(x, y, ...)
        panel.xyplot(rm, wp, type = "l", col = "gray50")
    })

