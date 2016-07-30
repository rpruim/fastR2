pal <- Pallets$pallets; dim(pal) <- c(5,4); pal
palperc <- 100 * row.perc(pal); palperc
Pallets$palperc <- as.vector(palperc)

