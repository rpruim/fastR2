expand.grid(run = paste("run", 1:6), rep = 1:1000) %>%
  mutate(x = rexp(6 * 1000)) %>%
  group_by(run) %>% arrange(rep) %>%
  mutate(runningMean = cumsum(x) / 1:length(x)) %>%
xyplot(runningMean ~ rep | run, data = ., 
       ylab = "running mean", xlab = "", type = "l",
       ylim = c(0, 3), 
       panel = function(...){ 
         panel.abline(h = 1, col = "gray70") 
         panel.xyplot(...) 
       })

