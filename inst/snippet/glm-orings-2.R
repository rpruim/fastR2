orings.model2 <-                  # link=logit is default, so unnecessary
    glm(cbind(damage, 6 - damage) ~ temp, data = orings, 
        family = binomial(link = logit))
summary(orings.model2)
p1 <- predict(orings.model, newdata = data.frame(temp = 31), type = 'response'); p1
p2 <- predict(orings.model2, newdata = data.frame(temp = 31), type = 'response'); p2
dbinom(0, 6, prob = p2)               # 0 damaged O-rings
xyplot(damage / 6 ~ temp, data = orings, 
    xlim = c(30, 100),
    ylim = c(0, 1),
    ylab = "percent of O-rings damaged",
    alpha = 0.7,
    panel = function(x, y, ...){
        panel.xyplot(temps, 
            predict(orings.model, type = 'response', 
                newdata = data.frame(temp = temps)),
            type = "l", lwd = 2, col = "gray50", lty = 2)
        panel.xyplot(temps, 
            predict(orings.model2, type = 'response', 
                newdata = data.frame(temp = temps)),
            type = "l", lwd = 2)
        panel.xyplot(x, y, ...)
    }
    )

