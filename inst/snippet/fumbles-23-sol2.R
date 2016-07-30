histogram(~ week2, data = Fumbles, width = 1)
plotDist("pois", lambda = mean( ~ week2, data = Fumbles), 
         add = TRUE, type = c("p", "l"))

