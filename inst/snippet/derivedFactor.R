HELP3 <- mutate(HELPrct, 
  risklevel = derivedFactor(
    low = sexrisk < 5, 
	medium = sexrisk < 10,
	high = sexrisk >= 10,
	.method = "first"      # use first rule that applies
	)
)
xyplot(sexrisk ~ risklevel, data = HELP3, jitter.x = TRUE, alpha = 0.4)

