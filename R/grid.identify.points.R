#' @export
#' @importFrom lattice trellis.par.set
#' @importFrom grid unit gpar grid.text
grid.identify.points <-
function (x, y, ind, offset = 0.03, labels = 1L:length(x), cex = 0.65, 
    col = lattice::trellis.par.get("add.text")$col, adj.x = TRUE, adj.y = FALSE) 
{
    if (adj.x) {
        hjust <- c(1, -1)[1 + as.numeric(x < mean(range(x)))]
        hoffset <- grid::unit(c(-1, 1)[1 + as.numeric(x < mean(range(x)))] * 
            offset, "npc")
    }
    else {
        hjust = rep(0, length(x))
        hoffset <- grid::unit(rep(0, length(x)), "npc")
    }
    if (adj.y) {
        vjust <- rep(1, length(x))
        voffset <- grid::unit(rep(offset, length(x)), "npc")
    }
    else {
        vjust = rep(0, length(x))
        voffset <- grid::unit(rep(0, length(x)), "npc")
    }
    grid::grid.text(as.character(labels[ind]), x = grid::unit(x[ind], "native") + 
        hoffset[ind], y = grid::unit(y[ind], "native") + voffset[ind], 
        gp = grid::gpar(cex = cex, col = col), hjust = hjust[ind], 
        vjust = vjust[ind], default.units = "native")
}
