kdeplot <- function(data, kernel, xlim, ylim,
    args = list(),
    lty = 1,
    lwd = 3,
    col = trellis.par.get('plot.line')$col,
    buffer = 0,
    n = 50,
    xlab = 'x',
    ylab = 'density',
    ...) {

    ndata <- length(data)
    scalingConstant = integrate( function(x) { 
        do.call( kernel, c(list(x), args) ) }, 
        -Inf, Inf)$value

    if (missing(xlim)) {
        xlim = range(data)
        buf <- buffer * diff(xlim)
        xlim <- xlim + c(-1, 1) * buf
    }

    if (missing(ylim)) {
        xvals <- seq(xlim[1], xlim[2], length = n)
        #yvals <- fun(xvals, unlist(args))
        yvals = do.call(kernel,
                      c(list(x = seq(xlim[1], xlim[2], length = 100)), args))
        ylim <- range(yvals)
        buf <- buffer * diff(ylim)
        ylim <- ylim + c(-1, 1) * buf
        
    }

    xyplot(ylim~xlim, xlab = xlab, ylab = ylab,
            panel = function(x, y, ...){ 
            panel.mathdensity(kde(data, kernel, ...),
                args = args,
                lwd = 3.5,
                lty = lty,
                col = col,
                n = n,
                ...) 
            for (d in data) {
            panel.mathdensity(
                dmath = function(x){
                    y <- do.call(kernel, c(list(x-d), args))
                    y / (ndata * scalingConstant)
                },
                args = args,
                lwd = 1.5,
                lty = 1,
                col = trellis.par.get('superpose.line')$col[2],
                n = n,
                ...) 
            }
            panel.rug(data, lwd = 2)
            },
            ...)
}
 
kdeplot(x, K1, xlim = c(1, 11), ylim = c(0, 0.32), n = 500,
    main = expression(K[1]))
kdeplot(x, K2, xlim = c(1, 11), ylim = c(0, 0.32), n = 500,
    main = expression(K[2]))
kdeplot(x, K3, xlim = c(1, 11), ylim = c(0, 0.32), n = 500,
    main = expression(K[3]))
kdeplot(x, K4, xlim = c(1, 11), ylim = c(0, 0.32), n = 500,
    main = expression(K[4]))

