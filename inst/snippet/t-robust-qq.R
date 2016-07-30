## ExpSims <-
##   expand.grid(n = c(10, 20, 40, 100), rep = 1:2000) %>%
##   group_by(n, rep) %>%
##   mutate(
##     pval = pval(t.test(rexp(n), mu = 1)),
##     dist = paste0("Exp(1); n=", n))
## 
## TSims <-
##   expand.grid(n = c(10, 20, 40, 100), rep = 1:2000) %>%
##   group_by(n, rep) %>%
##   mutate(
##     pval = pval(t.test(rt(n, df = 3), mu = 0)),
##     dist = paste0("t(3); n=", n))
## 
## xqqmath( ~ pval | dist, data = bind_rows(ExpSims, TSims),
##          dist = qunif, idline = TRUE, cex = 0.4, type = "l")
## xqqmath( ~ pval | dist, data = bind_rows(ExpSims, TSims),
##          dist = qunif, idline = TRUE, cex = 0.4, type = "l",
##          xlim = c(0, 0.2), ylim = c(0, 0.2))

