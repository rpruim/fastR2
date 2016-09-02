msummary(lm(weight ~ groove, data = TireWear))
xyplot(weight ~ groove, data = TireWear, type = c("p", "r"))

