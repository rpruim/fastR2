tally(t2d ~ genotype, fusion1m) 
chisq.test( tally( ~ t2d + genotype, fusion1m) )
chisq.test( tally( ~ t2d + (Tdose >= 1), fusion1m) )
chisq.test( tally( ~ t2d + (Tdose <= 1), fusion1m) )

