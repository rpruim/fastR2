gf_point(pallets ~ day, data = Pallets, 
         color = ~ employee) %>%
  gf_line(group = ~ employee)

