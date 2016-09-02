#' @export
panel.lm <-
function (x, y, model, fits, ...) 
{
    if (missing(fits)) {
        if (missing(model)) {
            model <- lm(y ~ x, ...)
        }
        fits <- fitted(model)
    }
    panel.xyplot(x[order(x)], fits[order(x)], type = "l", lwd = 2, 
        ...)
    panel.xyplot(x, y, ...)
}
#' @export
panel.smooth <-
function (x, y, col = trellis.par.get("plot.symbol")$col, col.smooth = trellis.par.get("add.line")$col, 
    bg = NA, pch = trellis.par.get("plot.symbol")$pch, cex = 1, 
    span = 2/3, iter = 3, ...) 
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, ...)
}
#' @export
panel.xyplotpoints <-
function (x, y, type = c("p"), ...) 
{
    panel.xyplot(x, y, type = c("p"), ...)
}
#' @export
panel.xyplotsmooth <-
function (x, y, type = c("p", "smooth"), ...) 
{
    panel.xyplot(x, y, type = c("smooth"), ...)
    panel.xyplot(x, y, type = c("p"), ...)
}
