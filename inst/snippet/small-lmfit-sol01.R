y <- c(0, 2, 0, 2, -1, 6)
x1 <- c(1, 1, 2, 2, 3, 3); x2 <- c(0, 1, 1, 2, 1, 3)
v0 <- rep(1, length(y))
v1 <- x1 - mean(x1); v2 = x2 - mean(x2)
w1 <- v1 - project(v1, v2)
w2 <- v2 - project(v2, v1)

