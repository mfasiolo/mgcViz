#' Plotting slice of higher-dimensional smooth effects
#' 
#' @description XXX
#' @param ... currently unused.
#' @name plot.mgcv.smooth.MD
#' @examples 
#' ## 3D example
#' library(mgcViz)
#' n <- 1e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n)
#' 
#' ob <- (x-z)^2 + (y-z)^2 + rnorm(n)
#' b <- gam(ob ~ s(x, y, z))
#' b <- getViz(b)
#' 
#' # Plot one 2D slice
#' plot( sm(b, 1), fix = c("z"=0) ) + l_fitRaster(noiseup = TRUE, mul = 3) + 
#'   l_fitContour(linetype = 2) + l_points(shape =  2)
#' 
#' ## 4D
#' n <- 5e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n); z2 <- rnorm(n)
#' 
#' ob <- (x-z)^2 + (y-z)^2 + z2^3 + rnorm(n)
#' b1 <- bam(ob ~ s(x, y, z, z2), discrete = TRUE)
#' b1 <- getViz(b1)
#' 
#' # Plot one 2D slice
#' plot(sm(b1, 1), fix = c("z"=0, "z2"=1)) + l_fitRaster() + l_fitContour()
#'
#' @rdname plot.mgcv.smooth.MD
#' @importFrom mvnfast maha
#' @importFrom stats cov quantile
#' @export plot.mgcv.smooth.MD
#' 
plot.mgcv.smooth.MD <- function(x, fix, n = 40, maxpo = 1e4,
                                too.far = c(0.1, NA), trans = function(.x){.x}, 
                                seWithMean = FALSE, 
                                unconditional = FALSE, ...) {

  if ( length(too.far) == 1 ){ too.far <- c(too.far, NA)  }
  
  # 1) Prepare data
  P <- .prepareP(o = x, unconditional = unconditional, residuals = TRUE, 
                 resDen = "none", se = TRUE, se.mult = 1, n = NULL, n2 = n,  
                 xlab = NULL, ylab = NULL, main = NULL, ylim = NULL, xlim = NULL,
                 too.far = too.far, seWithMean = seWithMean, fix = fix)

  # 2) Produce output object
  out <- .plot.mgcv.smooth.2D(x = P$smooth, P = P, trans = trans, maxpo = maxpo)
  
  class(out) <- c("plotSmooth", "2D", "gg")
  
  return(out)
}
