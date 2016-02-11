require(faraway); require(grid)
Stars <- faraway::star
star.plot1 <- xyplot(light~temp, data = Stars)
HotStars <- Stars %>% filter(temp > 3.7)   # select all but 4 coolest stars
star.model1 <- lm(light~temp, data = Stars)
star.model2 <- lm(light~temp, data = HotStars)
star.plot2 <- xyplot(light~temp, data = Stars,
    panel = function(x,y,...){
        panel.abline(reg=star.model1, lwd=2, lty=1,
            col=trellis.par.get('superpose.line')$col[2])
        panel.abline(reg=star.model2, lwd=2, lty=1,
            col=trellis.par.get('superpose.line')$col[1])
        panel.xyplot(x,y,...)
        ids <- which(Stars$temp < 4.0)
        grid.text(x=x[ids] + 0.04, y=y[ids],
            as.character(ids),
            default.units="native",gp=gpar(cex=0.7))
    })

