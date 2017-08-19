plot(balldrop.nls)
plot(balldrop.lm, w = 1)
gf_qq( ~ resid(balldrop.nls))
gf_qq( ~ resid(balldrop.lm))
gf_point(resid(balldrop.nls) ~ f(BallDrop$height))
gf_point(resid(balldrop.lm)  ~ g(BallDrop$height))

