mytheme <- function() {
    list(background = list(col = "transparent"), 
        plot.polygon = list(col = "navy"), 
        box.rectangle = list(col = "navy"), 
        box.umbrella = list(col = "navy"), 
        dot.line = list(col = "#e8e8e8"), 
        dot.symbol = list(col = "navy",pch=16), 
        plot.line = list(col = "navy",lwd=2), 
        plot.symbol = list(col = "navy",pch=16), 
        regions = list(col = heat.colors(100)), 
        reference.line = list(col = "#e8e8e8"), 
        superpose.line = list(lty=1:7,
                    col = c("navy","red","darkgreen","turquoise","orange",
                        "purple","pink","lightgreen")),
        superpose.symbol = list(pch = c(16,1, 3, 6, 0, 5, 17), 
            cex = rep(0.7, 7), 
            col = c("navy","red","darkgreen","turquoise","orange",
                        "purple","pink","lightgreen")),
        strip.background=list(alpha=1,
           col=c("#ffe5cc","#ccffff",
                 "#cce6ff","#ffccff","#ffcccc","#ffffcc")
            ),
        strip.shingle=list(alpha=1,
            col = c("#ff7f00","#00ffff",
                 "#0080ff","#ff00ff","#ff0000","#ffff00"))
        )
}

