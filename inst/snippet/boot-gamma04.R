gf_dhistogram( ~ boot.mean | sample, binwidth = 0.1, 
               data = Boot.Gamma %>% filter(sample == 1)) %>%
  gf_vline(xintercept = mean(S[[1]]))
gf_dhistogram( ~ boot.mean | sample, binwidth = 0.1, 
               data = Boot.Gamma %>% filter(sample == 2)) %>%
  gf_vline(xintercept = mean(S[[2]]))

