# compute the variance using E(X^2) - E(X)^2
value(integrate(f, k=2, lower = 0, upper = 2)) -
  value(integrate(f, k=1, lower = 0, upper = 2))^2    

