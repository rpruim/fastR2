Sims <-
  expand.grid(n=c(5, 10, 20, 50), rate = 1/10, rep = 1:2000) %>%
  group_by(n, rate, rep) %>%
  mutate(
    S = sum(rexp(n, rate)),
    U = S * rate,
    pvalS = 1 - pgamma(S, shape = n, rate = rate),
    pvalU = 1 - pgamma(U, shape = n, rate = 1)
  )
# We get the same p-value using either S or U as the test stat
xyplot(pvalS ~ pvalU, data = Sims, type = "l")

# The p-values exhibit the expected Unif(0,1) distribution
histogram( ~ pvalU | paste0("n=", n), data = Sims, width = 0.05, xlim = c(0,1))
qqmath( ~ pvalU | paste0("n=", n), data = Sims, dist = qunif)

# S, U approx Normal when sample size is large enough.
histogram( ~ U | paste0("n=", n), data = Sims)
histogram( ~ S | paste0("n=", n), data = Sims)

