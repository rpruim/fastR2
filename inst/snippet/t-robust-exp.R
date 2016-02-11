# an example CI from a sample of size 20
confint(t.test(rexp(20, 1/10)))
# 10,000 simulated samples of size 20
CIsim(n=20, samples=10000, estimand=10, rdist=rexp, args=list(rate=1/10))
#
# an example CI from a sample of size 5
confint(t.test(rexp(5, 1/10)))
# 10,000 simulated samples of size 5
CIsim(n=5, samples=10000, estimand=10, rdist=rexp, args=list(rate=1/10))
#
# an example CI from a sample of size 2
confint(t.test(rexp(2, 1/10)))
# 10,000 simulated samples of size 2
CIsim(n=2, samples=10000, estimand=10, rdist=rexp, args=list(rate=1/10))

