sampleSums <- replicate(5000, sum(runif(12, -0.5, 0.5)))
gf_qq( ~ sampleSums)
gf_dhistogram( ~ sampleSums, bins = 25)

