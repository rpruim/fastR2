## expand.grid(run = paste("run", 1:6), rep = 1:10000) %>%
##   mutate(x = rcauchy(6 * 10000)) %>%
##   group_by(run) %>% arrange(rep) %>%
##   mutate(runningMean = cumsum(x) / 1:length(x)) %>%
## xyplot(runningMean ~ rep | run, data = ., ylim = c(-10, 10),
##        ylab = "running mean", xlab = "", type = "l",
##        panel = function(...){
##          panel.abline(h = 0, col = "gray70")
##          panel.xyplot(...)
##        })

