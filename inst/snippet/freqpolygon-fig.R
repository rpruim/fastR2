gf_histogram( ~ Sepal.Length, data = iris, fill = "gray85", color = "black", bins = 20) %>%
  gf_freqpoly( ~ Sepal.Length, color = "black", size = 1, bins = 20)
gf_freqpoly( ~ Sepal.Length, color = ~ Species, data = iris, 
             binwidth = 0.5) 

