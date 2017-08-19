dd <- rcauchy(50)
qqdata <- data.frame( 
  x = c(runif(100), 10 - rexp(100), rchisq(100, df = 2), dd, jitter(-dd)),
  dist = rep(c("A", "B", "C", "D"), each = 100) 
)
gf_qq( ~ x, data = qqdata) %>%
  gf_facet_wrap( ~ dist, scales = "free") %>%
  gf_theme(axis.text = element_blank(), axis.ticks = element_blank())

