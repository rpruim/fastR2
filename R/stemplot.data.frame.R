stemplot.data.frame <-
function (x, ...) 
{
    doIt <- which(sapply(x, is.numeric))
    for (column in doIt) {
        cat(paste(rep("-", 25), sep = ""))
        cat(paste("\n", "Stemplot of ", names(x)[column], "\n\n", 
            sep = ""))
        print(stemplot.numeric(x[, column], ...))
    }
}
