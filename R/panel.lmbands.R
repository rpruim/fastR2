panel.lmbands <-
function (x, y, interval = "confidence", level = 0.95, model = lm(y ~ 
    x), pred.col = trellis.par.get("superpose.line")$col[2], 
    conf.col = trellis.par.get("superpose.line")$col[3], pred.lty = trellis.par.get("superpose.line")$lty[2], 
    conf.lty = trellis.par.get("superpose.line")$lty[3], ...) 
{
    x.to.fit <- seq(min(x), max(x), length = 40)
    confidence <- predict(model, interval = "confidence", newdata = data.frame(x = x.to.fit), 
        level = level)
    prediction <- predict(model, interval = "prediction", newdata = data.frame(x = x.to.fit), 
        level = level)
    panel.abline(lm(y ~ x))
    panel.xyplot(x.to.fit, prediction[, 2], type = "l", col = pred.col, 
        lty = pred.lty)
    panel.xyplot(x.to.fit, prediction[, 3], type = "l", col = pred.col, 
        lty = pred.lty)
    panel.xyplot(x.to.fit, confidence[, 2], type = "l", col = conf.col, 
        lty = conf.lty)
    panel.xyplot(x.to.fit, confidence[, 3], type = "l", col = conf.col, 
        lty = conf.lty)
    panel.xyplot(x, y, ...)
}
