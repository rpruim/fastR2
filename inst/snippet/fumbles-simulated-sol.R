sims <- data.frame(fumbles = rpois(120 * 4, 1.75), 
                   sample = rep(LETTERS[1:4], each=120))
favstats(~ fumbles | sample, data = sims)
histogram(~ fumbles | sample, data = sims, width=1, as.table=TRUE)
plotDist("pois", lambda = 1.75, add=TRUE)

