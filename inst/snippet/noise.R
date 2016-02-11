noise2 <- noise[noise$volume != 'none',]
model <- lm(score~volume*frequency,noise2)
anova(model)
xyplot(score~volume,groups=frequency,noise2,
    type='a',auto.key=list(lines=T,points=F,columns=2))
xyplot(score~(as.numeric(volume) + 0.05 * as.numeric(frequency)),
		groups=frequency,noise2, xlab="volume",
		scales=list(x=list(draw=F)))

