o
a <- sum(o[1:2]) / (2 * sum(o)); a
b <- sum(o[3:4]) / (2 * sum(o)); b
a + b                                # should equal 0.5
lnum <- 275 * log(a) + 211 * log(b)
ldenom <- sum(o * log (o/ sum(o)))
G <- -2 * (lnum - ldenom); G
1 - pchisq(G, df = 2)

