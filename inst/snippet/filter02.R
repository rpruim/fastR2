## gf_point(time_til_next ~ duration,
##        data = faithful2 %>% filter( duration > 3))
## # this one will use a different viewing window
## gf_point(time_til_next ~ duration, data = faithful2)  %>%
##   gf_lims(x = c(3, NA))

