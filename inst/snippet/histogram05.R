gf_histogram( ~ Sepal.Length, data = iris, bins = 15) %>%
  gf_facet_wrap( ~ ntiles(Sepal.Width, 4, format = "interval")) 

