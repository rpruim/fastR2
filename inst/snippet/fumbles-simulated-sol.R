Sims <- data.frame(fumbles = rpois(120 * 8, 1.75), 
                   sample = rep(LETTERS[1:8], each = 120))
 favstats( ~ fumbles | sample, data = Sims)
histogram( ~ fumbles | sample, data = Sims, width = 1, as.table = TRUE)
plotDist("pois", lambda = 1.75, add = TRUE)

