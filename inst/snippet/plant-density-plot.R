latticeExtra::useOuterStrips(
  stripplot( method ~ log(estimate/lambda) | sizeFac + lambdaFac, 
			data = results, jitter = TRUE, alpha = .1, as.table = TRUE)
)

