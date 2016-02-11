orings.plot <- xyplot(damage/6~temp, data=orings, 
    xlim=c(30,100),
    ylab="percent of O-rings damaged",
    alpha=0.7,
    panel=function(x,y,...){
        panel.xyplot(temps, 
            predict(orings.model,type='response', 
                newdata=data.frame(temp=temps)),
            type="l", lwd=2)
        panel.xyplot(x,y,...)
    }
    )
temps <- seq(30,100,by=2)
xyplot(failure~temp, data=orings, 
    xlim=c(30,100),
    ylab="probability of failure",
    alpha=0.7,
    panel=function(x,y,...){
        panel.xyplot(temps, 
            predict(orings.model,type='response', 
                newdata=data.frame(temp=temps)),
            type="l", lwd=2)
        panel.xyplot(x,y,...)
    }
    )

