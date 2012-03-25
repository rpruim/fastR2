pmfplot <-
function (pmf = dnorm, xlab, ylab, col, ...) 
{
    require(lattice)
    if (missing(col)) {
        col = "black"
    }
    if (missing(xlab)) {
        xlab = ""
    }
    if (missing(ylab)) {
        ylab = ""
    }
    xyplot(0 ~ 0, xlab = xlab, ylab = ylab, panel = function(x, 
        y, ...) {
        panel.mathdensity(pmf, col = col, ...)
    })
}
