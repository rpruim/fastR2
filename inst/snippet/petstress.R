pet.lm <- lm(Rate ~ Group, petstress)
summary(Rate~Group,petstress,fun=favstats)

