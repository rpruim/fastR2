# plant densities (lambda) for simulations
density <- c(0.01, 0.1, 0.25, 0.5, 1, 2, 4, 10, 100)       
sizes <- c(10, 20, 50)
simulate <- function(lambda = 1, size = 10, area = 1, method = c("count", "distance")){
	method <- match.arg(method) # allows for prefixes
	if (method == "count") {
		x <- rpois(size, lambda*area)
		plants <- sum(x)
		total.area <- size * area
		mle <- plants / total.area  
	} else {
		y <- rweibull( size, shape = 2, scale = 1/sqrt(pi * lambda) )
		plants <- length(y)
		total.area <- pi * sum(y^2)
		mle <- plants / total.area  
	}
	return( data.frame(
      size = size, lambda = lambda, method = method,
      estimate = mle, plants = plants, area = total.area,
      lambdaFac = paste0( "l=", lambda),
      sizeFac = paste0("size=", size)
    ) )
}

