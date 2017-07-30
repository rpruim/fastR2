x <- c(26.65, 28.03, 35.55, 29.30, 29.54, 36.20, 30.94, 
       23.69, 26.12, 27.13, 34.14, 30.51, 30.68, 29.46, 
       26.67, 36.51, 31.09, 20.74, 31.95, 27.01)

# Note the use of mapply() below.  
#   This tells R to use ALL the values of x with EACH combination of m and s.
Grid <-
  expand.grid(
    mean = seq(10, 100, by = 0.1),
    sd = seq(1, 10, by = 0.1)) %>% 
  mutate(
    loglik = mapply(function(m, s) { sum(dnorm(x, m, s, log = TRUE)) },
                    m = mean, s = sd))
Grid %>% arrange(-loglik) %>% head(3) 

