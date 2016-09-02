rat.lm <- lm(consumption ~ location + flavor, data = RatPoison)
anova(rat.lm)
plot(rat.lm, w=c(1, 2, 5))
xyplot(consumption ~ flavor, groups = location, data = RatPoison,
       type = c("p", "a"), 
       auto.key = list(points = TRUE, lines = TRUE))

