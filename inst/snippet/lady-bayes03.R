1 - xpbeta(c(0.5, 0.7, 0.9), shape1 = 19, shape2 = 3, 
           xlim = c(0.4, 1),
           refinements = list(
             labs(title = "Beta(19,3)"),
             scale_fill_brewer(type = "qual", palette = 3)
           ))
1 - xpbeta(c(0.5, 0.7, 0.9), shape1 = 10, shape2 = 2, 
           xlim = c(0.4, 1),
           refinements = list(
             labs(title = "Beta(9,2)"),
             scale_fill_brewer(type = "qual", palette = 3)
           ))


