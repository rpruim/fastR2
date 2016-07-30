xyplot(shape2 ~ shape1, data = Results, alpha = 0.4, 
	   panel = function(x, y, ...){
            panel.abline(a = 0, b = 5/2)
            panel.xyplot(x, y, ...)
            })
histogram( ~ shape2 / shape1, data = Results, type = "density", v = 2.5)

