utils::globalVariables(c(".index"))

#' Sum of Squares Plots
#' 
#' This function creates plots showing the "consumption" of residual
#' sum of squares resulting from adding predictors to a model.
#' 
#' @param model1 a linear model
#' @param model2 a linear model, often using [mosaic::rand()].
#' @param n an integer specifying how many times to regenerate 
#'   \code{model2}.
#' @param col1,col2,col3 Colors for the line segments in the plot
#' @param size1,size2,size3 Sizes of the line segments in the plot
#' @param ... additional arguments (currently ignored)
#' @param env an environment in which to evaluate the models.
#' @importFrom ggplot2 aes
## @importFrom dplyr tibble
#' 
#' @export
#' @examples
#'   
#' SSplot(
#'   lm(strength ~ limestone + water, data = Concrete),
#'   lm(strength ~ limestone + rand(7), data = Concrete),
#'   n = 50) 
#' \dontrun{
#' SSplot(
#'   lm(strength ~ water + limestone, data = Concrete),
#'   lm(strength ~ water + rand(7), data = Concrete),
#'   n = 1000) 
#' }
   
SSplot <-
  function(model1, model2, n = 1, 
           col1 = "gray50", size1 = 0.6,
           col2 = "navy", size2 = 1,
           col3 = "red", size3 = 1,
           ..., env = parent.frame()) {
    SS <- function(model, dfm) {
      dfe <- model$df.residual
      e <- model$effects[-1]
      n <- length(e)
      idx <- 0:length(e)
      idx <- union(idx[(idx + dfe) <= n], n)
      dplyr::tibble(df = idx, SS = cumsum(c(0,e^2))[1 + idx])
    }
    
    DD <- do(n) * SS(eval(model2$call, env))
    SSM1 <- SS(model1)
    ggplot2::ggplot() + 
      ggplot2::geom_line(
        data = dplyr::filter(DD, df <= model2$rank - 1) , 
        ggplot2::aes(x = df, y = SS, group = .index), color = col1,
        alpha = 1/sqrt(n), size = size1) +
      ggplot2::geom_line(
        data = dplyr::filter(SSM1, df <= model1$rank - 1),
        ggplot2::aes(x = df, y = SS), color = col2, alpha = 0.8, size = size2) +
      ggplot2::geom_line(
        data = dplyr::filter(SSM1, df >= model1$rank - 1),
        ggplot2::aes(x = df, y = SS), colour = col3, alpha = 0.8, size = size3) 
  }  
