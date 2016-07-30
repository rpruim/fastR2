x <- c(1, 1, 0, 4, 2, 1, 3, 0, 0, 2);  table(x)
mean(x) 
lrtStat <- function(x, lambda0 = 1) {
    x.bar <- mean(x); n <- length(x)
    2 * ( -n * x.bar + n * x.bar * log(x.bar) + n * lambda0 - n * x.bar * log(lambda0) )
    }
lrtStat(x)
pval <- 1 - pchisq(lrtStat(x), df = 1); pval

