Stars <- faraway::star
star.plot1 <- gf_point(light ~ temp, data = Stars)
# select all but 4 coolest stars
HotStars <- Stars %>% filter(temp > 3.7)   
star.model1 <- lm(light ~ temp, data = Stars)
star.model2 <- lm(light ~ temp, data = HotStars)
gf_point(light ~ temp, data = Stars) %>%
  gf_lm(color = "gray50", linetype = "dotted") %>%
  gf_lm(color = "red", linetype = "dashed", data = HotStars) %>%
  gf_text(light ~ (temp + 0.04), label = ~ as.character(id),
          data = Stars %>% mutate(id = 1:nrow(.)) %>% filter(temp < 4.0))

