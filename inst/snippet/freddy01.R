dbinom(20, 20, 0.8)            # probability of making all 20
1 - pbinom(14, 20, 0.8)        # probability of NOT making 14 or fewer
dbinom(16, 20, 0.8)            # probability of making exactly 16
gf_dist("binom", params = list(size = 20, prob = 0.8))

