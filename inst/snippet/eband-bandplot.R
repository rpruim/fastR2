eband.bandplot <- xyplot(distance~stretch,
    data=elasticband, ylim=c(80,210),
    panel=panel.lmbands,
    conf.lty=1,
    pred.lty=1
)

