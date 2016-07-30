# remove first few observations because of bad meter read
Ut2 <- Utilities %>% filter(year > 2000 | month > 6)
Ut2.lm <- lm(thermsPerDay ~ month + I(month^2), data = Ut2)
summary(Ut2.lm)
plotModel(Ut2.lm)
xplot(Ut2.lm, w=1:2)

