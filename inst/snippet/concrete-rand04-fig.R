SSplot(
  lm(strength ~ water + limestone, data = Concrete),
  lm(strength ~ water + rand(7), data = Concrete), n = 100) 
last_plot() + xlim(0, 2)

