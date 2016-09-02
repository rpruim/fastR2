xyplot(distance ~ projectileWt, 
    data = Trebuchet2, ylim = c(2.50, 10.50),
    panel = panel.lmbands, conf.lty = 1, pred.lty = 1
    )

