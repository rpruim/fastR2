# simulate 3 independent Norm(0,1) vars
Z1 <- rnorm(100000); Z2 <- rnorm(100000); Z3 <- rnorm(100000)
# create the X's from the Z's
X1 <- 1 + Z1; X2 <- 2 + Z1 + Z2; X3 <- 3 + Z1 + 2 * Z2 + Z3
data.frame( `E(X1)` = mean(X1), `E(X2)` = mean(X2),
            `Var(X1)` = var(X1), `Var(X2)` = var(X2),
            `Cov(X1, X2)` = cov(X1, X2), check.names = FALSE)  


