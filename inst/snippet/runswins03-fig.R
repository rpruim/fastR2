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
Aux_data <- 
  tibble(
   runmargin = seq(-3.5, 3.5, by = 0.1),
   winP = makeFun(bb.glm)(runmargin = runmargin)
  )
gf_point(winP ~ runmargin, data = BB) %>%
  gf_line(winP ~ runmargin, data = Aux_data, alpha = 0.4)

