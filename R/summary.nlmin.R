summary.nlmin <-
function (object, nsmall = 4, ...) 
{
    messages <- c("Relative gradient is close to zero, current iterate is probably an approximate solution.", 
        "Successive iterates within tolerance, current iterate is probably an approximate solution.", 
        "Last global step failed to locate a point lower than estimate. Either estimate is an approximate local minimum of the function or steptol is too small.", 
        "Iteration limit exceeded.", "Maximum step size stepmax exceeded five consecutive times. Either the function is unbounded below, becomes asymptotic to a finite value from above in some direction, or stepmax is too small.")
    cat(paste("\n       Minimum:", format(object$minimum, nsmall = nsmall)))
    cat("\n      Estimate:")
    cat(paste(format(object$estimate, nsmall = nsmall)))
    cat("\n      Gradient:")
    cat(paste(format(object$gradient, nsmall = nsmall)))
    cat(paste("\n    Iterations:", object$iterations))
    cat("\n\n")
    cat(paste(strwrap(messages[object$code]), collapse = "\n"))
    cat(paste("[Code=", object$code, "] ", sep = ""))
    cat("\n")
}
