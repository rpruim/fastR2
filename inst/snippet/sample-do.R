sample(vcd::VonBort, 10)           # SRS of size 10
# mean of SRS
mean( ~ deaths, data = sample(vcd::VonBort, 10))  
# mean of an iid random sample
mean( ~ deaths, data = resample(vcd::VonBort, 10))  
# means of 3 SRSs using do()
do (3) * mean(~ deaths, data = sample(vcd::VonBort, 10))
# means of 3 SRSs using replicate()
replicate(3, mean(~ deaths, data = sample(vcd::VonBort, 10)))
mean( ~ deaths, data = vcd::VonBort)    # mean of entire data set
histogram( ~ mean, 
           data = do (1000) * mean(~ deaths, data = sample(vcd::VonBort, 10)))

