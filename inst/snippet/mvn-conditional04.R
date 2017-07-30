X2cond <- X2[round(X1, 1) == 3]
X3cond <- X3[round(X1, 1) == 3]
data.frame(
  `mean(X2|X1 = 3)` = mean(X2cond), `mean(X3|X1 = 3)` = mean(X3cond),
  `var(X2|X1 = 3)` = var(X2cond), `var(X3|X1 = 3)` = var(X3cond),
  `cov(X2, X3 | X1 = 3)` = cov(X2cond, X3cond), check.names = FALSE
  )

