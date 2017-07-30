HotStars <- HotStars %>% 
  mutate(
    dfbeta_temp = dfbeta(star.model2)[, "temp"]
  )
gf_point(dfbeta_temp ~ index, data = HotStars) %>%
  gf_labs(y = "DFBETA") %>%
  gf_text(dfbeta_temp ~ (1.5 + index), 
          data = HotStars %>% filter(abs(dfbeta_temp) > 0.5),
          label = ~ as.character(index))
coef(lm(light ~ temp, HotStars))
coef(lm(light ~ temp, HotStars[-7, ]))

