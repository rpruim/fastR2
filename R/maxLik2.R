#' Augmented version of maxLik
#' 
#' This version of \code{\link{maxLik}} stores additional information in the 
#' returned object enabling a plot method.
#' 
#' 
#' @param loglik a log-likelihood function as for \code{\link{maxLik}}
#' @param ... additional arguments passed to \code{\link{maxLik}}
#' @param env an environment in which to evaluate \code{loglik}.
#' @importFrom numDeriv grad hessian
#' @importFrom maxLik maxLik hessian
#' @export

maxLik2 <- function(loglik, ..., env = parent.frame()) {
  orig.call <- match.call()
  fn <- loglik
  dots <- list(...)
  # clone of env
  env2 <- as.environment(as.list(env, all.names=TRUE))
  parent.env(env2) <- parent.env(env)
  
  result <- maxLik::maxLik(loglik, ...) 
  for (n in intersect(names(dots), names(formals(fn)))) {
    formals(fn)[[n]] <- NULL
    assign(n, dots[[n]], env2)
  }
  environment(fn) <- env2
  result$loglik <- fn
  class(result) <- c("maxLik2", class(result))
  result
}

#' plot method for augment maxLik objects
#' 
#' See \code{\link{maxLik2}} and \code{\link{maxLik}} for how to create
#' the objects this method prints.
#' 
#' @param x an object of class \code{"maxLik2"}
#' @param y ignored
#' @param ci a character vector with values among
#' \code{"Wald"} and \code{"likelihood"} specifying the type of 
#' interval to display
#' @param hline a logical indicating whether a horizontal line should 
#' be added 
#' @param ... additional arguments, currently ignored.

#' @importFrom stats coef filter 
#' @export

plot.maxLik2 <- function(x, y, ci = "Wald", hline = FALSE, ...) {
  ml <- x
  ci <- match.arg(tolower(ci), c("wald", "likelihood"), 
                  several.ok = TRUE)
  
  switch(
    length(coef(ml)), 
    "1" = {
      se <- stdEr(ml)
      S <- Vectorize(function(.x) {   numDeriv::grad(   Vectorize(ml$loglik), .x) })
      I <- Vectorize(function(.x) { - numDeriv::hessian(Vectorize(ml$loglik), .x) })
      Q <- function(.x) { ml$loglik(coef(ml)) - 1/2 * I(coef(ml)) * (.x - coef(ml))^2 }
      G <- data.frame(theta = c(coef(ml) - 3 * se, coef(ml) + 3 * se))
      p <- 
        ggplot(G, aes(x = theta)) + 
        stat_function(fun = ml$loglik, size = 1.2) +
        stat_function(fun = Q, colour = "skyblue", size = 0.8, alpha = 0.7) +
        geom_vline(xintercept = coef(ml), colour = "skyblue", linetype = "dotted", size = 1) +
          labs(x = names(coef(ml)), y = "log-likelihood")
        
        if ("wald" %in% ci) {
          p <- p + 
            geom_vline(xintercept = coef(ml) - 2 * se, colour = "skyblue", linetype = "dashed") +
            geom_vline(xintercept = coef(ml) + 2 * se, colour = "skyblue", linetype = "dashed") 
        }
      if ("likelihood" %in% ci || hline) {
        # create data frame of x, y pairs on the log-likelihood function
        D <- 
          data_frame(
            x = seq(coef(ml) - 4 * se, coef(ml) + 4 * se, length.out = 1000),
            y = suppressWarnings(ml$loglik(x))
          ) %>% 
          dplyr::filter(!is.na(y), !is.nan(y), is.finite(y)) %>%
          dplyr::filter(y > max(y, na.rm = TRUE) - 2)
      }
      
      if ("likelihood" %in% ci) {
        lo <- range(D$x)[1]
        hi <- range(D$x)[2]
        
        p <- p + 
          geom_vline(xintercept = lo, colour = "gray50", size = 0.5) +
          geom_vline(xintercept = hi, colour = "gray50", size = 0.5)
      }
      if (hline) {
        p <- p +
          geom_hline(yintercept = max(D$y, na.rm = TRUE) - 2, colour = "skyblue",
                     linetype = "dashed")
      }
      p
    },
    "2" = {
      se <- stdEr(ml)
      est <- coef(ml)
      G <- expand.grid(
        seq(est[1] - 3 * se[1], est[1] + 3 * se[1], length.out = 50),
        seq(est[2] - 3 * se[2], est[2] + 3 * se[2], length.out = 50)
      ) 
      names(G) <- c("p1", "p2")
      G$loglik <- apply(G, 1, ml$loglik)
      levelplot( loglik ~ p1 + p2, data = G, contour = TRUE, 
                 col.regions = grDevices::topo.colors(100),
                 xlab = names(coef(ml))[1],
                 ylab = names(coef(ml))[2]
      )
    },
    default = stop("Plotting only defined for likelihoods on 1 or 2 parameters")
  )
}

#' Information
#' 
#' Extract information from a maxLik object
#' 
#' @param object an object of class \code{"maxLik"}.
#' @param ... additional arguments
#' 
#' @export
information <- function(object, ...) {
  UseMethod("information")
}

#' @export
information.maxLik <- function(object, ...) {
  - maxLik::hessian(object)
}