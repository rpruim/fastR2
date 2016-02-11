Pitching2 <- filter(Pitching2005, GS > 4)
favstats(ERA~lgID, data = Pitching2)
bwplot(lgID~ERA, data = Pitching2)
histogram(~ERA | lgID, data = Pitching2, layout = c(1, 2), width = .2)

