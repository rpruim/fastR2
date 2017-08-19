gf_point(length ~ width, data = KidsFeet, color = ~ sex) %>% 
  gf_lm() %>%
  gf_refine(scale_color_manual(values = c(B = "navy", G = "red")))
gf_bar(~ sex, fill = ~ substance, position = "dodge", data = HELPrct) %>%
  gf_refine(scale_fill_brewer(type = "qual", palette = 3))

