Batting2 <- droplevels(filter(Batting, AB >= 200))
Batting2 <- mutate(Batting2, BA = H / AB)
favstats(BA ~ league, data = Batting2)
bwplot(league ~ BA, data = Batting2)
histogram( ~ BA | league, data = Batting2, layout = c(1, 2))

