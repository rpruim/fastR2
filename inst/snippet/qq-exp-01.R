## life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
## mean(life01); 1/mean(life01)
## qf <- function(x) { qexp(x, 1/mean(life01)) }
## qqmath(life01, distribution = qf)
## xqqmath(~life01, distribution = qf, idline = TRUE, qqmathline = FALSE)

