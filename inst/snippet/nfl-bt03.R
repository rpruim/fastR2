bta <- BTabilities(NFL.model)
nflRatings<- data.frame(
    team = rownames(bta),
    rating = bta[, "ability"],
    se = bta[, "s.e."],
    wins = as.vector(tally( ~ winner, data = NFL)),
    losses = as.vector(tally( ~ loser, data = NFL))
    )
row.names(nflRatings) <- NULL

nflRatings[rev(order(nflRatings$rating)), ]

