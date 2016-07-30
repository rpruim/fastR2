histogram(~ week3, data = Fumbles, width = 1)
plotDist("pois", lambda = mean( ~ week3, data = Fumbles), 
         add = TRUE, type = c("p", "l"))

