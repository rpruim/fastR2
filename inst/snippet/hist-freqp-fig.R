histogram( ~ Sepal.Length, data = iris,  
           panel = function(x, ...) {
             panel.xhistogram(x, ..., col = "gray95")
             panel.freqpolygon(x, lwd = 3, ...)
           } )

