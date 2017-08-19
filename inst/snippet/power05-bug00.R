## Power_data <-
##   expand.grid(n = 1:2000, p = c(0.52, 0.55, 0.60)) %>%
##   mutate(
##     plab = paste("alt prob =", as.character(p)),
##     critical = qbinom(0.025, size = n, prob = p),
##     power = 1 - (pbinom(n - critical + 1, n, p) - pbinom(critical - 1, n, p))
##   )
## gf_line(power ~ n | plab, data = Power_data) %>%
##   gf_labs(y = "power", x = "number of coin tosses") %>%
##   gf_lims(y = c(0, 1.1))

