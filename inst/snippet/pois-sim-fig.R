PoisSim <- 
  expand.grid(run = 1:10, i = 1:40) %>%
  group_by(run) %>%
  mutate(interval = rexp(40), time = cumsum(interval)) 
stop <- min(max(time ~ run, data = PoisSim))  # shortest run? 
stop <- 5 * trunc(stop / 5)                   # truncate to multiple of 5 
gf_point(run ~ time, data = PoisSim %>% filter(time <= stop), 
         shape = 1, size = 0.7, col = "black") %>%
  gf_hline(yintercept = seq(1.5, 9.5, by = 1), color = "gray60") %>%
  gf_vline(xintercept = seq(0, stop, by = 5), color = "gray60") %>%
  gf_theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())

