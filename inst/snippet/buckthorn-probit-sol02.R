dead2 <- makeFun(buck.model2) 
tbl2 <- 
  Buckthorn %>% 
  group_by(conc) %>%
  summarise( 
    total = length(dead),
    obsDead = sum(dead),
    obsAlive = sum(!dead),
    propDead = sum(dead) / length(dead)
  ) %>%
  mutate(
    expDead = dead2(conc) * total,
    expAlive = (1 - dead2(conc)) * total,
    expPropDead = dead2(conc)
  )
tbl2
xyplot(propDead ~ conc, data = tbl2,
    ylab = "predicted death rate", xlab = "concentration of glyphosate",
)
plotFun(dead2(c) ~ c, add = TRUE)
plotFun(dead(c) ~ c, add = TRUE, col = "gray60", lwd = 3, lty = 2)

