BB <- 
  BB %>% 
  mutate( 
    winP = W / G, 
    predWinP = makeFun(bb.glm)(runmargin), 
    winPdiff = winP - predWinP
    ) 
BB %>% arrange(-abs(winPdiff)) %>% select(1, 22:24) %>% head()

gf_point(winP ~ predWinP, data = BB) %>%
  gf_abline(slope = 1, intercept = 0)

