rate = 1/10
v = (1 / rate)^2                                   # var of exponential
mu = 10                                            # mean of exponential
zci(rexp(20, rate), sd = sqrt(v))$conf.int         # an example CI
#
# 10,000 simulated samples of size 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, 
      rdist = rexp, args = list(rate = rate), estimand = mu, 
      method = zci, method.args = list(sd = sqrt(v)))

