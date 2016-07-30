dbinom(0, 4, 0.05)             # P(all four bits received correctly
pbinom(1, 7, 0.05)             # P(>= 6 of 7 bits received correctly)
p <- seq(0, 1, by = 0.01)
DD <- data.frame(
  probability = c(dbinom(0, 4, p), pbinom(1, 7, p)),
	error.rate = c(p, p),
	method = rep(c("plain", "Hamming"), each = length(p)))
xyplot(probability ~ error.rate, data = DD,
       groups = method, type = "l",
       xlab = "bitwise error rate", 
       ylab = "message error rate",
       auto.key = list(columns = 2, lines = TRUE, points = FALSE))

