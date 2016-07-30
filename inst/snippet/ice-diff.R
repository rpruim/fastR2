surface <- Ice[Ice$location=='surface', c("treatment", "t1930")]
intra <- Ice[Ice$location == 'intramuscular', "t1930"]
newdata <- cbind(surface, intra)
names(newdata) <- c('treatment', 'surfTemp', 'intraTemp')
anova(lm(surfTemp - intraTemp ~ treatment, data = newdata))

