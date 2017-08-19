binom.test(25, 70)  # Clopper-Pearson
binom.test(25, 70, ci.method = "Wald")
binom.test(25, 70, ci.method = "score")
 prop.test(25, 70)  # also uses inverted score test

