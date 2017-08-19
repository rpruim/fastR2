BinomGrid <- 
  expand.grid(pi = seq(0, 1, by = 0.001)) %>% 
  mutate(
    prior = 1,    # uniform prior
    likelihood = dbinom(20, size = 50, prob = pi),
    posterior = prior * likelihood                 # kernel of posterior
  ) 
posterior_sample <-
  with(BinomGrid, sample(pi, size = 1e5, prob = posterior, replace = TRUE))

# credible interval
cdata(~posterior_sample, 0.95)   # central 95% credible interval
# compare with analytical result above
gf_dhistogram( ~ posterior_sample, binwidth = 0.02, alpha = 0.4) %>%
  gf_dist("beta", shape1 = 21, shape2 = 31, color = "navy")
qbeta(c(0.025, 0.975), shape1 = 21, shape2 = 31)

