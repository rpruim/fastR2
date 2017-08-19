# median of posterior
qbeta(0.5, shape1 = 19, shape2 = 3)
qbeta(0.5, shape1 = 10, shape2 = 2)

# median of posterior -- approxiamted by sampling
median(posterior_sample20)
median(posterior_sample10)

# mean of posterior -- approximated by sampling
mean(posterior_sample20)
mean(posterior_sample10)

# MAP
nlmax(function(x) dbeta(x, shape1 = 19, shape2 = 3), p = 0.5) %>% value()
nlmax(function(x) dbeta(x, shape1 = 10, shape2 = 2), p = 0.5) %>% value()

