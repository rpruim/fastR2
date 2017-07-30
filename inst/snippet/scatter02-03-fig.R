# Two ways to add facets to a scatter plot
gf_point(Sepal.Length ~ Sepal.Width | Species, data = iris)
gf_point(Sepal.Length ~ Sepal.Width, data = iris) %>%
  gf_facet_wrap( ~ Species)
  last_plot() %>% gf_refine(scale_x_continuous(breaks = 2:5))
gf_point(Sepal.Length ~ Sepal.Width, data = iris,
         color = ~ Species, shape = ~ Species, alpha = 0.7)
  last_plot() %>% gf_refine(scale_x_continuous(breaks = 2:5))

