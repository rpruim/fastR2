PosteriorSample <-   
  sample(NormalGrid, size = 1e5, replace = TRUE, 
         prob = NormalGrid$posterior)

