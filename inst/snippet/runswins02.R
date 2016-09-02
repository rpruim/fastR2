BB <- 
  BB %>% 
  mutate( 
    winP = W / G, 
    predWinP = makeFun(glm.bb)(runmargin), 
    winPdiff = winP - predWinP
    ) 
BB %>% arrange(-abs(winPdiff)) %>% select(1, 22:24) %>% head()

xyplot(winP ~ predWinP, data = BB,
    panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        panel.abline(0, 1)
    })

