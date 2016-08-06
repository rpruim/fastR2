val <- c(0,1,2,3,4)
frequency<- c(9,2,3,0,1)
n <- sum(frequency); n
x.bar <- sum(val * frequency) / n; x.bar
v <- sum(frequency * (val - x.bar)^2) / (n - 1); v
T <- 14 * v / x.bar; T
1- pchisq(T, 14)

