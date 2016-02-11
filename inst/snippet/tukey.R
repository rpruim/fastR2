useOuterStrips(
xyplot( Y~X | paste('a=',a,sep="") + paste("b=",b,sep=""),
            ddd, groups=original,
            scales=list(relation='free', draw=FALSE))
)

