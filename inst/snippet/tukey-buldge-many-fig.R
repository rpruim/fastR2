ddd <- ddd %>% 
  mutate(
    afacet = factor(paste("a=", a, sep = "")),
    bfacet0 = factor(paste("b=", b, sep = "")),
    bfacet = factor(bfacet0, levels = rev(levels(bfacet0)))
  )
         
gf_point(Y ~ X, data = ddd, color = ~original) %>%
  gf_facet_grid(bfacet ~ afacet, scale = "free") %>%
  gf_theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none") 

