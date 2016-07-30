sim_pval <- function(n, alt = 0.55, null = 0.5) {
  pval(binom.test(rbinom(1, n, alt), n, prob = null))
}
Sims200 <- do(1000) * sim_pval(n = 200)
Sims400 <- do(1000) * sim_pval(n = 400)
Sims1075 <- do(1000) * sim_pval(n = 1075)
tally( ~ p.value < 0.05, data = Sims200, format = "proportion")
tally( ~ p.value < 0.05, data = Sims400, format = "proportion")
tally( ~ p.value < 0.05, data = Sims1075, format = "proportion")

