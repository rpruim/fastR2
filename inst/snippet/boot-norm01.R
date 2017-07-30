S <- lapply(1:6, function(x) {rnorm(36, 100, 12)})
Boots <-
  bind_rows(
    (do(3000) * c(boot.mean = mean(resample(S[[1]])))) %>% 
      mutate(sample = "1", sample.mean = mean(S[[1]])),
    (do(3000) * c(boot.mean = mean(resample(S[[2]])))) %>% 
      mutate(sample = "2", sample.mean = mean(S[[2]])),
    (do(3000) * c(boot.mean = mean(resample(S[[3]])))) %>% 
      mutate(sample = "3", sample.mean = mean(S[[3]])),
    (do(3000) * c(boot.mean = mean(resample(S[[4]])))) %>% 
      mutate(sample = "4", sample.mean = mean(S[[4]])),
    (do(3000) * c(boot.mean = mean(resample(S[[5]])))) %>% 
      mutate(sample = "5", sample.mean = mean(S[[5]])),
    (do(3000) * c(boot.mean = mean(resample(S[[6]])))) %>% 
      mutate(sample = "6", sample.mean = mean(S[[6]]))
  )

