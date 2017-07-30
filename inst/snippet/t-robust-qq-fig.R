ExpSims <-
  expand.grid(n = c(10, 20, 40, 80), rep = 1:2000) %>%
  group_by(n, rep) %>%
  mutate(
    pval = pval(t.test(rexp(n), mu = 1)), 
    dist = paste0("Exp(1); n=", n))

TSims <-
  expand.grid(n = c(10, 20, 40, 80), rep = 1:2000) %>%
  group_by(n, rep) %>%
  mutate(
    pval = pval(t.test(rt(n, df = 3), mu = 0)), 
    dist = paste0("t(3); n=", n))

gf_qq( ~ pval, data = bind_rows(ExpSims, TSims), 
       distribution = qunif, geom = "line") %>%
  gf_abline(slope = 1, intercept = 0, color = "red", 
            linetype = "dashed", alpha = 0.6) %>% 
  gf_facet_wrap( ~ dist, nrow = 2)
gf_qq( ~ pval, data = bind_rows(ExpSims, TSims), na.rm = TRUE,
       distribution = qunif, geom = "line") %>% 
  gf_abline(slope = 1, intercept = 0, color = "red", 
            linetype = "dashed", alpha = 0.6) %>%
  gf_lims(x = c(0, 0.2), y = c(0, 0.2)) %>%
  gf_facet_wrap( ~ dist, nrow = 2)

