randomData <- rbinom(n = 30, size = 4, prob = 0.5)
randomData
tally( ~ randomData)
vals <- setNames(0:4, 0:4)             # add labels for nicer displays below
dbinom(vals, size = 4, prob = 0.5)     # matches earlier example 
dbinom(vals, size = 4, prob = 0.5) * 30  # pretty close to our table above
pbinom(vals, size = 4, prob = 0.5)       # same as cumsum(dbinom(...))
qbinom(0.20, size = 20, prob = 0.5)   
pbinom(7, 20, 0.5)                    #  < 0.20
pbinom(8, 20, 0.5)                    # >= 0.20

