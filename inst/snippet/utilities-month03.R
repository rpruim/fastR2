ut.lm4a <- 
  lm(thermsPerDay ~ poly(monthShifted, 2), data = Utilities3)
msummary(ut.lm4a)
favstats( ~ (fitted(ut.lm4a) - fitted(ut.lm4)))

