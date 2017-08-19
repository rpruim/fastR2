CoolingWater3 <- read.csv("data-raw/CoolingWater.csv") %>% mutate(condition = "control")
CoolingWater4 <- read.csv("data-raw/CoolingWaterOil.csv") %>% mutate(condition = "oil")
CoolingWater1 <- read.csv("data-raw/CoolingWater2.csv") %>% 
  mutate(condition = "control",
         time = round(time, 0),
         temp = round(temp, 1)) 
CoolingWater2 <- read.csv("data-raw/CoolingWaterOil2.csv") %>% 
  mutate(condition = "oil",
         time = round(time, 0),
         temp = round(temp, 1)) 

gf_point(temp ~ time, data = CoolingWater1, color = ~"1") %>%
  gf_point(temp ~ time, data = CoolingWater2, color = ~"2") 
gf_point(temp ~ time, data = CoolingWater3, color = ~"3") %>%
  gf_point(temp ~ time, data = CoolingWater4, color = ~"4") 

devtools::use_data(CoolingWater1, CoolingWater2, CoolingWater3, CoolingWater4, overwrite = TRUE)
