kde <- function(data, kernel = K1, ...) {
    n <- length(data)
    scalingConstant = integrate(function(x){kernel(x, ...)}, -Inf, Inf)$value
    function(x) {
        mat <- outer(x, data, FUN = function(x, data) {kernel(x - data, ...)} )
        val <- rowSums(mat)
        val <- val / (n * scalingConstant)
        return(val)
    }
}

