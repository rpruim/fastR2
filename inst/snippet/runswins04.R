Aux_data <- 
  data_frame(
   runmargin = seq(-3.5, 3.5, by = 0.1),
   winP = makeFun(bb.glm)(runmargin = runmargin)
  )
gf_point(winP ~ runmargin, data = BB) %>%
  gf_line(winP ~ runmargin, data = Aux_data, alpha = 0.4)

