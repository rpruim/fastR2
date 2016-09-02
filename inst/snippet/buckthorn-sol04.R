# calculate the percentage dead at each concentration used.
tbl <- 
  Buckthorn %>%
  group_by(conc) %>%
  summarise( 
    total = n(),
    obsDead = sum(dead),
    obsAlive = sum(!dead),
    propDead = obsDead / (obsDead + obsAlive)
  ) %>%
  mutate( 
    expDead = dead(conc) * total,
    expAlive = (1 - dead(conc)) * total,
    expPropDead = dead(conc)
  )
tbl
xyplot(propDead ~ conc, data = tbl,
    ylab = "predicted death rate", xlab = "concentration of glyphosate",
)
plotFun(dead(c) ~ c, add = TRUE)

