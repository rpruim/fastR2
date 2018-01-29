altfstats <- function(x) {
    cat(paste("  mean:", format(mean(x), 4), "\n"))
    cat(paste("median:", format(median(x), 4), "\n"))
    cat(paste("    sd:", format(sd(x), 4), "\n"))
}
altfstats((1:20)^2)

