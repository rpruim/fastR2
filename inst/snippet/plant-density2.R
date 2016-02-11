results <- c()
for (lambda in density) {
  for (size in sizes) {
    results <- rbind(results, 
	  do(1000) * simulate(lambda, size, method = "count"), 
	  do(1000) * simulate(lambda, size, method = "distance") 
	) 
  }
}

