m <- max(~week1, data = Fumbles)
tally(~factor(week1,levels=0:m), data = Fumbles)
favstats(~week1, data = Fumbles)

