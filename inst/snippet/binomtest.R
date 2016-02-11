## power <- function(n, p_alt, alpha = 0.05, p_null = 0.50) {
##   critical_low <- qbinom(    alpha/2, size=n, prob=p_null) - 1
##   critical_hi  <- qbinom(1 - alpha/2, size=n, prob=p_null) + 1
##   pbinom(critical_low, n, p_alt) + 1 - pbinom(critical_hi - 1, n, p_alt)
## }
## 
## PowerData <-
##   expand.grid(n=seq(5, 10000, by=5), p_alt = c(0.52, 0.55, 0.60)) %>%
##   mutate(
##     power = power(n, p_alt),
##     plab = paste("alt prob =", as.character(p_alt))
##   )
## 
## xyplot(power ~ n | plab, data = PowerData,
##        ylab="power", xlab="number of coin tosses",
##        ylim=c(0, 1.1), type='l', lwd=2)

