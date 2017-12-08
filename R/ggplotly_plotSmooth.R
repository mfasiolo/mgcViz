#############################################
#' Convert plotSmooth objects to plotly
#' 
#' @description Given an object of class \code{plotSmooth}, this function
#'              extract a \code{ggplot} object and use it a
#' 
#' @param p an object of class \code{plotSmooth}.
#' @param ... extra arguments passed to [plotly::ggplotly].
#' @name ggplotly.plotSmooth
#' @importFrom plotly ggplotly
#' @usage ggplotly.plotSmooth(p, ...)
#' @examples 
#' \dontrun{
#' library(mgcViz)
#' n  <- 1e3
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' dat <- data.frame("x1" = x1, "x2" = x2,
#'                   "y" = sin(x1) + 0.5 * x2^2 + pmax(x2, 0.2) * rnorm(n))
#' b <- bam(y ~ s(x1)+s(x2), data = dat, method = "fREML", discrete = TRUE)
#' b <- getViz(b)
#' 
#' # Interactive plotly plot.
#' ggplotly.plotSmooth( plot( sm(b, 1) ) + l_points(colour = "grey") + 
#'                      l_fitLine() + l_ciLine() )
#' }
#' @rdname ggplotly.plotSmooth
#' @export ggplotly.plotSmooth
#' @export
#'
ggplotly.plotSmooth <- function(p, ...){
  
  ggplotly(p = p$ggObj, ...)
  
}