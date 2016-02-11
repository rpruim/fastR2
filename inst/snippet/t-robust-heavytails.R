# an example CI from a sample of size 20
confint(t.test(rt(20, 3)))
# 10,000 simulated samples of size 20
CIsim(n=20, samples=10000, estimand=0, rdist=rt, args=list(df=3))
#
# an example CI from a sample of size 5
confint(t.test(rt(5, 3)))
# 10,000 simulated samples of size 5
CIsim(n=5, samples=10000, estimand=0, rdist=rt, args=list(df=3))
#
# an example CI from a sample of size 2
confint(t.test(rt(2, 3)))
# 10,000 simulated samples of size 2
CIsim(n=2, samples=10000, estimand=0, rdist=rt, args=list(df=3))

