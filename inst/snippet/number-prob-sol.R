t <- tally( ~ number, data = LittleSurvey); t
histogram(~number, LittleSurvey, width = 1)
max(t)
which(t == max(t))
which(t == min(t))
tally(~ (number %% 2 == 0), data = LittleSurvey)

