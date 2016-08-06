tally(t2d ~ genotype, data = Fusion1m) 
chisq.test(tally( ~ t2d + genotype, data = Fusion1m))
chisq.test(tally( ~ t2d + (Tdose >= 1), data = Fusion1m))
chisq.test(tally( ~ t2d + (Tdose <= 1), data = Fusion1m))

