set.seed(1234)
expand.grid(run = paste("run", 1:6), rep = 1:10000) %>%
  mutate(x = rcauchy(6 * 10000)) %>%
  group_by(run) %>% arrange(rep) %>%
  mutate(runningMean = cumsum(x) / 1:length(x)) %>%
gf_line(runningMean ~ rep | run) %>%
  gf_hline(yintercept = 0, color = "red", alpha = 0.5) %>%
  gf_lims(y = c(-20, 20)) %>%
  gf_labs(y = "running mean", x = "") 

