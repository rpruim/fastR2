CPS85 <- mutate(CPS85, workforce.years = age - 6 - educ)
favstats(~workforce.years, data = CPS85)

