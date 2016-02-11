coinTosses <- data.frame(outcome=rbinom(1000,1,0.5),toss=1:1000)
coinTosses$relFreq <- cumsum(coinTosses$outcome) / coinTosses$toss
xyplot(relFreq~toss,coinTosses,type="l",
        panel=function(x,y,...){
            panel.abline(h=0.5,lty=1,col="gray80")
            panel.xyplot(x,y,...)
        },
#        main="Results of 1000 simulated coin tosses",
        lwd=2,
        ylim=c(0,1),
        ylab='relative frequency',
        xlab="number of tosses")

