## PoisSim <-
##   expand.grid(run = 1:10, i = 1:40) %>%
##   group_by(run) %>%
##   mutate(interval = rexp(40), time = cumsum(interval))
## stop <- min(max(time ~ run, data = PoisSim))  # shortest run?
## stop <- 5 * trunc(stop / 5)                   # truncate to multiple of 5
## stripplot(run ~ time, data = PoisSim %>% filter(time <= stop),
##           pch = 1, cex = .7, col = "black",
## 	panel = function(x, y, ...){
## 		panel.abline(h = seq(1.5, 9.5, by = 1), col = "gray60")
## 		panel.abline(v = seq(0, stop, by = 5), col = "gray60")
## 		panel.stripplot(x, y, ...) })

