m <- max( ~ week2, data = Fumbles)
tally( ~ factor(week2, levels = 0:m), data = Fumbles)
favstats( ~ week2, data = Fumbles)
m <- max( ~ week3, data = Fumbles)
tally( ~ factor(week3, levels = 0:m), data = Fumbles)
favstats( ~ week3, data = Fumbles)

