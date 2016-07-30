histogram( ~ sigma^2, data = MLEs, width = 5)
xqqmath( ~ sigma^2, data = MLEs, distribution = function(p) qchisq(p, df=39),
         xlab = "Chisq(39)", ylab = expression(hat(sigma)^2))

