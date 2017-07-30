# 90% credible interval for the mean
cdata( ~ mu, data = PosteriorSample, p = 0.90) 
rethinking::HPDI(PosteriorSample$mu, 0.90)
# 90% CI for comparision
t.test(x, conf.level = 0.90) %>% confint()
# 90% credible interval for the standard deviation
cdata( ~ sigma, data = PosteriorSample, p = 0.90)
rethinking::HPDI(PosteriorSample$sigma, 0.90)

