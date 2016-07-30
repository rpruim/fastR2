n <- 20  
x <- runif(n, 2, 10)
y <- exp(0.3 * x)
e <- exp(rnorm(n, 0, 0.1))
y <- y * e
foo <- function(x) {
    a <- x[2]
    x <- x[1]
    if (a == 0) { return (log(x)) }
    return (x^a)
}

power <- function(x, a) {
    M <- cbind(x, a)
    return  (apply(M, 1, foo))
}
powers <- c(0, 0.5, 1, 2,3); np <- length(powers)
a <- rep(rep(powers, each = n), each = np)
b <- rep(rep(powers, each = n), times = np)
x <- rep(x, times = n * np)
y <- rep(y, times = n * np)
X <- power(x, a)
Y <- power(y, b)
original <- (a==1 & b==1)
ddd <- data.frame(X = X, Y = Y, a = a, b = b, original = original)
xyplot(y ~ x)

