darts.lm <- lm(Accuracy~Distance*Hand,darts)
anova(darts.lm)
xyplot(Accuracy~Distance,groups=Hand,darts,
                type='a',auto.key=list(lines=T,points=F,columns=2))
xyplot(Accuracy~Distance,groups=Hand,darts,
                auto.key=list(lines=F,points=T,columns=2))

