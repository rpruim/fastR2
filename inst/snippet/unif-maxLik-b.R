# works if we select a good starting point -- but warns about boundary issues.
maxLik(loglik, start = 10, x = x)
maxLik(loglik, start = 10, x = x, method = "NM")
maxLik(loglik, start = 10, x = x, method = "BFGS")
# but some starting points don't work well...
maxLik(loglik, start = 8, x = x)
maxLik(loglik, start = 8, x = x, method = "NM")
maxLik(loglik, start = 8, x = x, method = "BFGS")

