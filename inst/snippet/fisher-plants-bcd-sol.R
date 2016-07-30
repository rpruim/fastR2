# test a specific value of theta vs. best possible theta
testTheta <- 
  Vectorize(
    vectorize.args = "theta0",
    function(theta0, x) {
      w <- 2 * (loglik.fisher(theta.hat, x) - loglik.fisher(theta0, x))
      p.value <- 1 - pchisq(w, df = 1)
      return(c(theta0 = theta0, w = w, p.value = p.value))
    }
  )
testTheta(c(0.03, 0.05, 0.07), x = fisher.counts) %>% t() %>% data.frame()

