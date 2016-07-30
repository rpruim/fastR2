xyplot(distance ~ projectileWt,
    data = Trebuchet2, ylim = c(250, 1050),
    panel = panel.lmbands, conf.lty = 1, pred.lty = 1
    )

