gf_histogram( ~ Sepal.Length, data = iris, bins = 15) %>%
  gf_facet_wrap( ~ ntiles(Sepal.Width, 4, format = "interval")) 
gf_histogram( ~ Sepal.Length | Species, bins = 15,
              data = iris %>% filter(Species == "virginica"))

