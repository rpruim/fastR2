sampleSums <- replicate(2000, sum(runif(12, -0.5, 0.5)))
qqmath(~sampleSums)
histogram(~sampleSums)

