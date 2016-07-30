t.test(TireWear$weight, TireWear$groove, paired = T)
t.test(TireWear$weight - TireWear$groove)
t.test( ~ (weight - groove), data = TireWear)

