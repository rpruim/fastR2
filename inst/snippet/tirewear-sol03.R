t.test(TireWear$weight, TireWear$groove, paired = TRUE)
t.test( ~ (weight - groove), data = TireWear)

