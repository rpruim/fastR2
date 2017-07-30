# Two ways to add facets to a scatter plot
gf_point(Sepal.Length ~ Sepal.Width | Species, data = iris)
gf_point(Sepal.Length ~ Sepal.Width, data = iris) %>%
  gf_facet_wrap( ~ Species)

