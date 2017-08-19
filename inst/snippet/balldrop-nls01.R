balldrop.nls <- 
  nls(time ~ alpha0 + alpha1 * height^d, 
      data = BallDrop, 
      start = list(alpha0 = 0, alpha1 = 1, d = 1))

