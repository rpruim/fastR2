Footballs <- HeliumFootballs %>% mutate(diff = helium - air)
Footballs %>% head(3)
t.test( ~ diff, data = Footballs)

