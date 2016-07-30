xyplot(score ~ noise, groups = group, data = MathNoise,
    type='a', auto.key = list(lines=T, points=F, columns=2))
xyplot(score ~ noise, groups = group, data = MathNoise,
    auto.key = list(lines=F, points=T, columns=2))
xyplot(score ~ group, groups = noise, data = MathNoise,
    type='a', auto.key = list(lines=T,points=F,columns=2))
xyplot(score ~ group, groups = noise, data = MathNoise,
    auto.key = list(lines=F,points=T,columns=2))

