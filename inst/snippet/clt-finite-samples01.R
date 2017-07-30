x <- c(1, 6, 6, 8, 9)
mu <- sum(x * 0.2); mu                 # population mean
v <- sum(x^2 * 0.2) - mu^2; v          # population variance
pairsums <- outer(x, x, "+")           # compute 25 sums
pairmeans <- pairsums / 2

