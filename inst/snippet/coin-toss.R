coinTosses <- data.frame(
  outcome = rbinom(1000, 1, 0.5),
  toss = 1:1000) %>% 
  mutate(relFreq = cumsum(outcome) / toss)
gf_hline(yintercept = 0.5, col = "skyblue", size = 1, alpha = 0.8) %>%
  gf_line(relFreq ~ toss, data = coinTosses)  %>%
  gf_labs(title =  "Results of 1000 simulated coin tosses",
          y = "relative frequency",
          x = "number of tosses") %>%
  gf_lims(y = c(0, 1))

