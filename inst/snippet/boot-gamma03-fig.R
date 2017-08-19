gf_dhistogram( ~ boot.mean | sample, data = Boot.Gamma, 
               binwidth = 0.1) %>%
  gf_dist("gamma", rate = 16, shape = 32) 

