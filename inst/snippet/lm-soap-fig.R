daysToFit <- seq(1,22,by=0.5)
linfits <- predict(soap.model1,newdata=data.frame(Day=daysToFit))
transfits <- predict(soap.model2,newdata=data.frame(Day=daysToFit))^3
soap.plot <- xyplot(Weight~Day,data=soap, 
    panel = function(x,y,...) {
        panel.xyplot(daysToFit,linfits, lwd=2, type="l", 
            col=trellis.par.get('superpose.line')$col[1])
        panel.xyplot(daysToFit,transfits, lwd=2, type="l",
            col=trellis.par.get('superpose.line')$col[2])
        panel.xyplot(x,y,cex=1.0,...)
    }
    )

