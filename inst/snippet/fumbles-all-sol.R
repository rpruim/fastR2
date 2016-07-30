Fumbles <- Fumbles %>% mutate(all = week1 + week2 + week3)
histogram(~ all, data = Fumbles, width = 1,  main= "All fumbles weeks 1-3")
plotDist("pois", lambda = mean( ~ all, data = Fumbles), add = TRUE)

