## BetaSims <-
##   expand.grid(rep = 1:1000, size = c(5, 10, 20, 40)) %>%
##   group_by(rep, size) %>%
##   mutate(sample.mean = mean(rbeta(size, 0.5, 0.5)))
## gf_qq( ~ sample.mean | factor(size), data = BetaSims) %>%
##   gf_facet_wrap( ~ factor(size), scales = "free")
## gf_dhistogram( ~ sample.mean | factor(size), data = BetaSims, bins = 25)

