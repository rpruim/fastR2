life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
mean(life01); 1 / mean(life01)
qe <- function(x) { qexp(x, 1/mean(life01)) } 
xqqmath( ~ life01, distribution = qe, idline = TRUE, qqmathline = FALSE)
xqqmath( ~ life01, distribution = qexp, qqmathline = FALSE)

