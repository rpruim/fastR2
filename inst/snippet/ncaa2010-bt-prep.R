NCAA <- NCAAbb %>% 
  filter(season == "2009-10", !postseason) %>%
  mutate(
    neutralSite = grepl("n", notes, ignore.case = TRUE), # at neutral site?
    homeTeamWon = hscore > ascore)                     # did home team win?
# remove teams that didn't play >= 5 at home and >=5 away
# (typically div II teams that played a few div I teams)
h <- tally( ~ home, data = NCAA); a <- tally( ~ away, data = NCAA)
deleteTeams <- c(names(h[h <= 5]), names(a[a <= 5]))
NCAA <- NCAA %>% 
  filter(!(home %in% deleteTeams | away %in% deleteTeams))

