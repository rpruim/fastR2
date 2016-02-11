t.test(~ (Vitamin - Placebo), data=endurance)
t.test(~ (log(Vitamin) - log(Placebo)) , data=endurance)
t.test(~ (log(Vitamin / Placebo)) , data=endurance)  # same as above
t.test(~(Vitamin/Placebo), data=endurance)
t.test(~ (1/Vitamin - 1/Placebo), data=endurance)
x <- sum(~ (Vitamin > Placebo), data=endurance)
n <- nrow(endurance)
binom.test(x, n)
prop.test(x, n)

