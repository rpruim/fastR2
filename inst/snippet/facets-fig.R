gf_density( ~ age, data = HELPrct, fill = ~ sex) %>%
  gf_facet_grid( substance ~ .)
gf_density( ~ age, data = HELPrct, fill = ~ sex) %>%
  gf_facet_grid( substance ~ ., scales = "free_y", space = "free")
gf_density( ~ age, data = HELPrct, fill = ~ sex) %>%
  gf_facet_wrap( ~ substance, ncol = 1)  
HELPrct %>% select(age, mcs, i1, cesd, substance) %>%
  gather(variable, value, age:cesd) %>%
  gf_dens( ~ value, color = ~substance) %>%
  gf_facet_wrap( ~ variable, scales = "free", ncol = 2)

