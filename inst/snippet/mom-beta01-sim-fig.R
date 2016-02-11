results <- do(1000) * beta.mom(rbeta(50, 2, 5))
histogram(~shape1, results, type='density', v=2)
histogram(~shape2, results, type='density', v=5)
xyplot(shape2~shape1, data=results, 
	   panel=function(x, y, ...){
            panel.abline(a=0, b=5/2)
            panel.xyplot(x, y, ...)
       })
histogram(~shape2/shape1, results, type='density', v=2.5)

