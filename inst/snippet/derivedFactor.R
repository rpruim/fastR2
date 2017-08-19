HELP3 <-
  mutate(
    HELPrct, 
    risklevel = derivedFactor(
      low = sexrisk < 5, 
      medium = sexrisk < 10,
      high = sexrisk >= 10,
      .method = "first"      # use first rule that applies
    )
  )
gf_jitter(sexrisk ~ risklevel, data = HELP3, 
          height = 0.2, width = 0.3, alpha = 0.4)

