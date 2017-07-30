expand.grid(run = paste("run", 1:6), rep = 1:1000) %>%
  mutate(x = rexp(6 * 1000)) %>%
  group_by(run) %>% arrange(rep) %>%
  mutate(runningMean = cumsum(x) / 1:length(x)) %>%
gf_line(runningMean ~ rep | run) %>%
  gf_hline(yintercept = 1, color = "red", alpha = 0.5) %>%
  gf_labs(y = "running mean", x = "") %>%
  gf_lims(y = c(0, 3)) 

