#' Plotting slice of higher-dimensional smooth effects
#' 
#' @description This function plots a 2D slice of a higher-dimensional smooth effects.
#' @param x a smooth effect object, extracted using [mgcViz::sm].
#' @param fix a named vector indicating which variables must be kept fixed and to what values.
#'            When plotting a smooth in (d+2) dimensions, then d variables must be fixed.
#' @param n sqrt of the number of grid points used to compute the effect plot.
#' @param xlim if supplied then this pair of numbers are used as the x limits for the plot.
#' @param ylim if supplied then this pair of numbers are used as the y limits for the plot.
#' @param maxpo maximum number of residuals points that will be used by layers such as
#'              \code{resRug()} and \code{resPoints()}. If number of datapoints > \code{maxpo},
#'              then a subsample of \code{maxpo} points will be taken.
#' @param too.far a numeric vector with two entries. The first has the same interpretation 
#'                as in [plot.mgcv.smooth.2D] and it avoids plotting the smooth effect
#'                in areas that are too far form any observation. The distance will be calculated only
#'                using the variables which are not in \code{fix} (see above). Hence in two dimensions,
#'                not in the full d+2 dimensions. Set it to -1 to plot the whole
#'                smooth. The second entry determines which residuals and covariates pairs are closed
#'                enough to the selected slice. If left to \code{NA} on the 10\% of points which are
#'                closest (in terms of scaled Euclidean distance) to the current slice will be plotted.
#'                Set it to -1 to plot all the residuals.
#' @param trans monotonic function to apply to the smooth and residuals, before plotting.
#'              Monotonicity is not checked. 
#' @param unconditional if \code{TRUE} then the smoothing parameter uncertainty corrected covariance 
#'                      matrix is used to compute uncertainty bands, if available.
#'                      Otherwise the bands treat the smoothing parameters as fixed.
#' @param seWithMean if TRUE the component smooths are shown with confidence intervals that 
#'                   include the uncertainty about the overall mean. If FALSE then the uncertainty
#'                   relates purely to the centred smooth itself. Marra and Wood (2012) suggests 
#'                   that TRUE results in better coverage performance, and this is also suggested 
#'                   by simulation.
#' @param ... currently unused.
#' @return An objects of class \code{plotSmooth}.
#' @references Marra, G and S.N. Wood (2012) Coverage Properties of Confidence Intervals for 
#'             Generalized Additive Model Components. Scandinavian Journal of Statistics.
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
plot.mgcv.smooth.MD <- function(x, fix, n = 40, xlim = NULL, ylim = NULL, maxpo = 1e4,
                                too.far = c(0.1, NA), trans = identity, 
                                seWithMean = FALSE, 
                                unconditional = FALSE, ...) {

  if ( length(too.far) == 1 ){ too.far <- c(too.far, NA)  }
  
  # 1) Prepare data
  P <- .prepareP(o = x, unconditional = unconditional, residuals = TRUE, 
                 resDen = "none", se = TRUE, se.mult = 1, n = NULL, n2 = n,  
                 xlab = NULL, ylab = NULL, main = NULL, ylim = ylim, xlim = xlim,
                 too.far = too.far, seWithMean = seWithMean, fix = fix)

  # 2) Produce output object
  out <- .plot.mgcv.smooth.2D(x = P$smooth, P = P, trans = trans, maxpo = maxpo)
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
}
