daysToFit <- seq(1, 22, by = 0.5)
linfits <- predict(Soap.model1, newdata = data.frame(day = daysToFit))
transfits <- predict(Soap.model2, newdata = data.frame(day = daysToFit))^3
xyplot(weight ~ day, data = Soap, 
    panel = function(x, y, ...) {
        panel.xyplot(daysToFit, linfits, lwd = 2, type = "l", 
            col = trellis.par.get('superpose.line')$col[1])
        panel.xyplot(daysToFit, transfits, lwd = 2, type = "l",
            col = trellis.par.get('superpose.line')$col[2])
        panel.xyplot(x, y, cex = 1.0, ...)
    }
)

