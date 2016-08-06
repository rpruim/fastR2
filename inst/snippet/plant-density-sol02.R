Results <- c()
for (lambda in density) {
  for (size in sizes) {
    Results <- bind_rows(Results, 
	  do(1000) * simulate(lambda, size, method = "count"), 
	  do(1000) * simulate(lambda, size, method = "distance") 
	) 
  }
}

