require(tidyverse)
Ice2 <- 
  Ice %>% 
  gather("key", "temp", b0:r12000) %>% 
  separate(key, c("phase", "time"), sep = 1) %>% 
  mutate(time = readr::parse_number(time), subject = as.character(subject))  
Ice2 %>% filter(phase == "t") %>% 
  gf_line(temp ~ time, group = ~ subject, color = ~sex) %>%
  gf_facet_grid( treatment ~ location, scales = "free_x") %>%
  gf_labs(
    title = "Temperature during treatment phase (3 conditions, 2 locations)")

