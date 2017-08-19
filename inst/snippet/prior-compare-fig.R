CoinGrid <- 
  expand.grid(
    pi = seq(0, 1, by = 0.001), 
    prior_name = 
      c("Unif(0, 1)", "Beta(20, 10)", "Tri(0.3, 0.7)", "Unif(0.3, 0.7)")) %>%
  mutate(
    prior = 
      case_when(
        prior_name == "Unif(0, 1)"     ~ dunif(pi, 0, 1), 
        prior_name == "Beta(20, 10)"   ~ dbeta(pi, 20, 10), 
        prior_name == "Tri(0.3, 0.7)"  ~ 
          triangle::dtriangle(pi, a = 0.3, b = 0.7, c = 0.5), 
        prior_name == "Unif(0.3, 0.7)" ~ dunif(pi, 0.3, 0.7)
      ),
    likelihood = dbinom(38, size = 100, prob = pi)) %>% 
  group_by(prior_name) %>%
  mutate(posterior =  prior * likelihood / sum(prior * likelihood) * 1000)

gf_line(prior ~ pi, color = ~ "prior", data = CoinGrid) %>%
  gf_line(posterior ~ pi, color = ~ "posterior", data = CoinGrid) %>%
  gf_facet_wrap( ~ prior_name) %>%
  gf_theme(legend.position = "top") %>% 
  gf_refine(guides(color = guide_legend("distribution: ")))

