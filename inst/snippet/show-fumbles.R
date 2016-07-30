histogram( ~ week1, data = Fumbles)
plotDist("pois", lambda = mean( ~ week1, data = Fumbles), 
         add = TRUE, type = c("p", "l"))

