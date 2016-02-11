xplot(concrete.lm0,which=2)
xplot(concrete.lm0,which=3)
xplot(concrete.lm0,which=5,add.smooth=FALSE)
xyplot(resid(concrete.lm1) ~ fitted(concrete.lm1),
               main = "residuals vs fits",
               ylab = "residuals",
               xlab = "fitted values",
               sub  = "lm(strength ~ limestone + water)" )

