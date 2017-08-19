step.lm <- lm(HR - restHR ~ height * freq, data = Step)
msummary(step.lm)
anova(step.lm)
gf_line(HR - restHR ~ freq, data = Step, color = ~height, 
        group = ~ height, stat = "summary", fun.data = mean_se) %>%
  gf_jitter(width = 0.15, height = 0)

