temps <- seq(30, 100, by = 2)
xyplot(damage / 6 ~ temp, data = orings, 
    xlim = c(30, 100),
    ylim = c(-0.05, 1.05),
    ylab = "percent of O-rings damaged",
    alpha = 0.7,
    panel = function(x, y, ...){
        panel.xyplot(temps, 
            predict(orings.model, type = "response", 
                newdata = data.frame(temp = temps)),
            type = "l", lwd = 2)
        panel.xyplot(x, y, ...)
    }
    )
xyplot(failure ~ temp, data = orings, 
    xlim = c(30, 100),
    ylim = c(-0.05, 1.05),
    ylab = "probability of failure",
    alpha = 0.7,
    panel = function(x, y, ...){
        panel.xyplot(temps, 
            predict(orings.model, type = "response", 
                newdata = data.frame(temp = temps)),
            type = "l", lwd = 2)
        panel.xyplot(x, y, ...)
    }
    )

