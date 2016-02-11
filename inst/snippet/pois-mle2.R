# generate 5000 samples of size 10
rdata <- do(5000) * rpois(10, 1)   
statTally(x, rdata, lrtStat)

