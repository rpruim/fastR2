# merge fusion1 and pheno keeping only id's that are in both
Fusion1m <- merge(FUSION1, Pheno, by = "id", all = FALSE)

