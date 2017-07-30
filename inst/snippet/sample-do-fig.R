sample(vcd::VonBort, 10)           # SRS of size 10
# mean of SRS
c(mean.deaths = mean( ~ deaths, data = sample(vcd::VonBort, 10)))
# mean of an iid random sample
c(mean.deaths = mean( ~ deaths, data = resample(vcd::VonBort, 10)))
# means of 3 SRSs using do()
do (3) * c(mean.deaths = mean(~ deaths, data = sample(vcd::VonBort, 10)))
# means of 3 SRSs using replicate()
replicate(3, mean(~ deaths, data = sample(vcd::VonBort, 10)))
mean( ~ deaths, data = vcd::VonBort)    # mean of entire data set
gf_dhistogram( ~ mean.deaths, binwidth = 0.1, 
  data = do (2000) * 
    c(mean.deaths = mean(~ deaths, data = sample(vcd::VonBort, 10)))) %>%
  gf_vline(xintercept = 0.7)

