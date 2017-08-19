# P(X1 = 20 & X2 = 30 & X3 = 50)
dmultinom(c(20, 30, 50), size = 100, prob = c(0.2, 0.3, 0.5))
# 1 column for each of 10 random draws from Multinom(100, <0.2, 0.3, 0.5>)
rmultinom(10, size = 100, prob = c(0.2, 0.3, 0.5))
# create a data frame with 1000 draws
SimMultinom <-
  rmultinom(2000, size = 100, prob = c(0.2, 0.3, 0.5)) %>% 
  t() %>% data.frame() 
head(SimMultinom, 3)
# scatter plot shows the negative correlation between X1 and X2
gf_point(X2 ~ X1, data = SimMultinom, alpha = 0.2) %>%
  gf_density2d() %>%
  gf_labs(x = expression(X[1]), y = expression(X[2]))

