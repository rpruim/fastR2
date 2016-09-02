xyplot(score ~ noise, groups = group, data = MathNoise,
    type = c("p", "a"), jitter.x = TRUE,
    auto.key = list(lines = TRUE, points = TRUE, columns = 2))
xyplot(score ~ group, groups = noise, data = MathNoise,
    type = c("p", "a"), jitter.x = TRUE,
    auto.key = list(lines = TRUE, points = TRUE, columns = 2))

