require(fastR2)
names(Batting)
Batting2005 <- Batting %>% filter(year == 2005)
df_stats(HR ~ team | league, data = Batting2005, max)

gf_histogram( ~ AB | league, data = Batting2005)
gf_point(HR ~ H, data = Batting2005 %>% filter(team == "DET"))
gf_boxplot(HR ~ league, data = Batting2005) %>%
  gf_refine(coord_flip())

