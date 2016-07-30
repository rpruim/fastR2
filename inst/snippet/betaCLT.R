## BetaSims <-
##   expand.grid(rep = 1:1000, size = c(5, 10, 20, 40)) %>%
##   group_by(rep, size) %>% mutate(mean = mean(rbeta(size, 0.5, 0.5)))
## qqmath(    ~ mean | factor(size), data = BetaSims,
##            scales = list(relation = "free"))
## histogram( ~ mean | factor(size), data = BetaSims, n = 25, density = TRUE)

