qplot( y = log(estimate/lambda), x = method, 
	   colour = method, fill = method, 
	   data = results, 
       geom = c("violin"), alpha = I(.5)) +
  annotate("hline", yintercept = 0) + coord_flip() +  
  facet_grid( lambdaFac ~ sizeFac ) + theme_bw()

latticeExtra::useOuterStrips( 
  bwplot( method ~ log(estimate / lambda) | sizeFac * lambdaFac,
    data = results, 
    groups = method,
    panel = panel.violin,
    auto.key = TRUE, as.table = TRUE)
)

