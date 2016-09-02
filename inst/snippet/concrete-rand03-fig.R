SSplot(
  lm(strength ~ limestone + water, data = Concrete),
  lm(strength ~ limestone + rand(7), data = Concrete), n = 1000) 
last_plot() + xlim(0, 2)

