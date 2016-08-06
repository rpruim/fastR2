NCAA <- NCAA2010 %>% 
  mutate(
    neutralSite = grepl('n', notes, ignore.case = TRUE), # at a neutral site?
    homeTeamWon = hscore > ascore)                       # did home team win?
# remove teams that didn't play >= 5 at home and >=5 away
# (typically div II teams that played a few div I teams)
h <- tally( ~ home, data = NCAA); a <- tally( ~ away, data = NCAA)
deleteTeams <- c(names(h[h <= 5]), names(a[a <= 5]))
NCAA <- NCAA %>% 
  filter(!(NCAA$home %in% deleteTeams | NCAA$away %in% deleteTeams))
# remove unused levels from home and away factors
teams <- union(NCAA$home, NCAA$away)
NCAA$home <- factor(NCAA$home, levels = teams)
NCAA$away <- factor(NCAA$away, levels = teams)

