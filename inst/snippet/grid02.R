Grid2 <-
  expand.grid( mean = seq(29.4, 29.8, by = 0.001),
               sd = seq(3.8, 4.2, by = 0.001)) %>% 
  mutate(loglik = mapply(function(m, s) { sum(dnorm(x, m, s, log = TRUE)) },
                      m = mean, s = sd))
Grid2 %>% arrange(-loglik) %>% head(3) 

