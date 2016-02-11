x <- seq(0,10,by=0.25)
resid <- rnorm(length(x))
y1 <- x + resid
y2 <- (x-5)^2/5 + resid
y3 <- x + (0.75 + 0.25 *x) *resid
y4 <- x + resid
y4[35] <- x[35]- 2*max(abs(resid))
resid1 <- resid(lm(y1~x))
resid2 <- resid(lm(y2~x))
resid3 <- resid(lm(y3~x))
resid4 <- resid(lm(y4~x))
stresid1 <- resid1 / sd(resid1)
stresid2 <- resid2 / sd(resid2)
stresid3 <- resid3 / sd(resid3)
stresid4 <- resid4 / sd(resid4)
fit1 <- fitted(lm(y1~x))
fit2 <- fitted(lm(y2~x))
fit3 <- fitted(lm(y3~x))
fit4 <- fitted(lm(y4~x))
group <- rep(toupper(letters[1:4]),each=length(x))
rdata <- data.frame(
            x = rep(x,times=4),
            fit=c(fit1,fit2,fit3,fit4),
            residual=c(resid1,resid2,resid3,resid4),
            stresidual=c(stresid1,stresid2,stresid3,stresid4),
            group=group)
xyplot(stresidual~fit|group,
                    data=rdata,
                    ylab="residual",
                    scales=list(x=list(relation="free"),draw=F),
                    ylim=c(-1.1,1.1) * max(abs(rdata$stresidual)),
                    as.table=T)

