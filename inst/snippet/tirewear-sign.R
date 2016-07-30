x <- sum( ~ (weight > groove), data = TireWear)
n <- nrow(TireWear)
binom.test(x, n)
prop.test(x, n)
binom.test(~ (weight > groove), data = TireWear)
prop.test(~ (weight > groove), data = TireWear)

