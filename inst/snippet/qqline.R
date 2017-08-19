gf_qq( ~ age | substance, data = HELPrct, alpha = 0.4) %>%
  gf_qqline(color = "red") %>%
  gf_qqline(color = "skyblue", tail = 0.10)

