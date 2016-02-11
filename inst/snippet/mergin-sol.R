FUSION1m2 <- 
  FUSION1 %>%
  merge(Pheno, by.x = 'id', by.y = 'id', all.x = FALSE, all.y = FALSE)
Pheno %>%  left_join(FUSION1, by = "id") %>% dim()
Pheno %>% inner_join(FUSION1, by = "id") %>% dim()

