t.test( ~ (vitamin - placebo), data = Endurance)
t.test( ~ (log(vitamin) - log(placebo)) , data = Endurance)
t.test( ~ (log(vitamin / placebo)) , data = Endurance)  # same as above
t.test( ~ (vitamin / placebo), data = Endurance)
t.test(~ (1 / vitamin - 1 / placebo), data = Endurance)
binom.test(~ (vitamin > placebo), data = Endurance)
prop.test(~ (vitamin > placebo), data = Endurance)

