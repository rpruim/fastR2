MoreTosses <- data.frame(heads = rbinom(1000, 1000, 0.5))
gf_histogram( ~ (heads / 1000), data = MoreTosses, binwidth = 0.005) %>%
  gf_labs(title =  "Results of 1000 simulations\nof 1000 coin tosses",
          x = "proportion heads") %>%
  gf_lims(x =  c(0.44, 0.56))
LotsMoreTosses <- data.frame(heads = rbinom(1000, 10000, 0.5))
gf_histogram( ~ (heads / 10000), data = LotsMoreTosses, binwidth = 0.002) %>%
  gf_labs(title =  "Results of 1000 simulations\nof 10,000 coin tosses",
          x = "proportion heads") %>%
  gf_lims(x =  c(0.44, 0.56))

