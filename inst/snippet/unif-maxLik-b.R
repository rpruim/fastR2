# works if we select a good starting point -- but warns about boundary issues.
maxLik(loglik.unif, start = 10, x = x)
maxLik(loglik.unif, start = 10, x = x, method = "NM")
maxLik(loglik.unif, start = 10, x = x, method = "BFGS")
# but some starting points don't work well...
maxLik(loglik.unif, start = 8, x = x)
maxLik(loglik.unif, start = 8, x = x, method = "NM")
maxLik(loglik.unif, start = 8, x = x, method = "BFGS")

