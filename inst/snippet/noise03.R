gf_jitter(score ~ noise, color = ~ group, data = MathNoise,
          width = 0.15, height = 0) %>%
  gf_line(stat = "summary", group = ~ group, fun.data = mean_se)

gf_jitter(score ~ group, color = ~ noise, data = MathNoise,
          width = 0.15, height = 0) %>%
  gf_line(stat = "summary", group = ~ noise, fun.data = mean_se)

