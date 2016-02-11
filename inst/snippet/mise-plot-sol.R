require(ggplot2)
qplot( y = mise, x = adj, data = transform(results, adj = factor(round(adjust, 2))),  
       colour = kernel, alpha = I(.7), group = kernel,
       geom = "line") + 
  facet_grid( dist ~ size , scales = "free_y")       

