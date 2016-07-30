tally(t2d ~ genotype, Fusion1m) 
chisq.test( tally( ~ t2d + genotype, Fusion1m) )
chisq.test( tally( ~ t2d + (Tdose >= 1), Fusion1m) )
chisq.test( tally( ~ t2d + (Tdose <= 1), Fusion1m) )

