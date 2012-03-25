CIsim <-
function (n, samples = 100, rdist = rnorm, args = list(), estimand = 0, 
    conf.level = 0.95, method = t.test, method.args = list(), 
    interval = function(x) {
        do.call(method, c(list(x, conf.level = conf.level), method.args))$conf.int
    }, estimate = function(x) {
        do.call(method, c(list(x, conf.level = conf.level), method.args))$estimate
    }, verbose = TRUE) 
{
    sampleData <- replicate(samples, do.call(rdist, c(list(n = n), 
        args)))
    lower <- apply(sampleData, 2, function(x) {
        interval(x)[1]
    })
    upper <- apply(sampleData, 2, function(x) {
        interval(x)[2]
    })
    estimate <- apply(sampleData, 2, function(x) {
        estimate(x)
    })
    cover <- as.integer(estimand >= lower & estimand <= upper)
    cover <- factor(cover, levels = c(0, 1), labels = c("No", 
        "Yes"))
    cis <- data.frame(lower = lower, upper = upper, estimate = estimate, 
        cover = cover, sample = 1:samples)
    if (verbose) {
        cat("Did the interval cover?")
        print(table(cis$cover)/samples)
    }
    invisible(cis)
}
