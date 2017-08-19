rat.lm <- lm(consumption ~ location + flavor, data = RatPoison)
anova(rat.lm)
plot(rat.lm, w=c(1, 2, 5))
gf_point(consumption ~ flavor, color = ~ location, data = RatPoison,
          width = 0.15, height = 0) %>%
  gf_line(stat = "summary", group = ~ location, fun.data = mean_se) 

