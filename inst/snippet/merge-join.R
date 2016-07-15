# merge FUSION1 and Pheno keeping only id's that are in both
FUSION1m <- merge(FUSION1, Pheno, by.x = 'id', by.y = 'id', 
                  all.x = FALSE, all.y = FALSE)
head(FUSION1m, 3)
left_join( Pheno, FUSION1, by = "id") %>% dim()
inner_join( Pheno, FUSION1, by = "id") %>% dim()
# which ids are only in \dataframe{Pheno}?
setdiff(Pheno$id, FUSION1$id)   

