results <- do(1000) * beta.mom(rbeta(50, 2, 5))
histogram(~shape1, results, type='density', v=2)
histogram(~shape2, results, type='density', v=5)

