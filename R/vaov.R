#' ANOVA vectors
#' 
#' Compute vectors associated with 1-way ANOVA
#' 
#' This is primarily designed for demonstration purposes to show how 1-way
#' ANOVA models partition variance.  It may not work properly for more
#' complicated models.
#' 
#' @aliases vaov vaov.formula
#' @param x a formula.
#' @param data a data frame.
#' @param \dots additional arguments.
#' @return A data frame with variables including \code{grandMean},
#' \code{groupMean}, \code{ObsVsGrand}, \code{STotal}, \code{ObsVsGroup},
#' \code{SError}, \code{GroupVsGrand}, and \code{STreatment}. The usual SS
#' terms can be computed from these by summing.
#' @author Randall Pruim
#' @keywords stats
#' @export
#' @examples
#' 
#' aov(pollution ~ location, airpollution)
#' vaov(pollution ~ location, airpollution)
#' 
vaov <-
function (x, ...) 
{
    UseMethod("vaov", x)
}
