favstats(yield ~ treatment, data = Corn2)
Corn.model <- lm(yield ~ treatment, data = Corn2)
msummary(Corn.model)

