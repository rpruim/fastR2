gf_dhistogram( ~ boot.mean | sample, binwidth = 0.5, 
               data = Boots %>% filter(sample == 1)) %>%
  gf_lims(x = c(88,112)) %>%
  gf_vline(xintercept =  mean(S[[1]])) %>%
  gf_labs(x = "3000 bootstrap means from sample 1") 

gf_dhistogram( ~ boot.mean | sample, binwidth = 0.5, 
               data = Boots %>% filter(sample == 2)) %>%
  gf_lims(x = c(88,112)) %>%
  gf_vline(xintercept =  mean(S[[2]])) %>%
  gf_labs(x = "3000 bootstrap means from sample 2") 

