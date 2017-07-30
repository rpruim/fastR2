gf_point(velocity^2 ~ force.drag, data= Drag, color = ~ factor(height))
plot(model1, w = 1)
gf_point(velocity ~ force.drag, data = Drag,  color = ~ factor(height)) %>%
  gf_refine(scale_x_log10(), scale_y_log10()) 
plot(model3, w = 1)

