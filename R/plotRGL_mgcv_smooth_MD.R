#' Visualizing a 2D slice of a smooth effects in 3D 
#' 
#' @description This method plots an interactive 3D representation of a 2-dimensional
#'              slice of a multi-dimensional smooth effect, using the rgl package.
#'              
#' @param x a smooth effect object, extracted using [mgcViz::sm].
#' @param fix a named vector indicating which variables must be kept fixed and to what values.
#'            When plotting a smooth in (d+2) dimensions, then d variables must be fixed.
#' @param se when TRUE (default) upper and lower surfaces are added to the plot at \code{se.mult} 
#'           (see below) standard deviations for the fitted surface.
#' @param n sqrt of the number of grid points used to compute the effect plot.
#' @param residuals if TRUE, then the partial residuals will be added.
#' @param type the type of residuals that should be plotted. See [residuals.gamViz].
#' @param maxpo maximum number of residuals points that will be plotted. 
#'              If number of datapoints > \code{maxpo}, then a subsample of \code{maxpo} points will be taken.
#' @param too.far a numeric vector with two entries. The first has the same interpretation 
#'                as in [plot.mgcv.smooth.2D] and it avoids plotting the smooth effect
#'                in areas that are too far form any observation. The distance will be calculated only
#'                using the variables which are not in \code{fix} (see above). Hence in two dimensions,
#'                not in the full d+2 dimensions. Set it to -1 to plot the whole
#'                smooth. The second entry determines which residuals and covariates pairs are closed
#'                enough to the selected slice. If left to \code{NA} on the 10\% of points which are
#'                closest (in terms of scaled Euclidean distance) to the current slice will be plotted.
#'                Set it to -1 to plot all the residuals.
#' @param xlab if supplied then this will be used as the x label of the plot.
#' @param ylab if supplied then this will be used as the y label of the plot.
#' @param main used as title for the plot if supplied.
#' @param xlim if supplied then this pair of numbers are used as the x limits for the plot.
#' @param ylim if supplied then this pair of numbers are used as the y limits for the plot.
#' @param se.mult a positive number which will be the multiplier of the standard errors 
#'                when calculating standard error surfaces. 
#' @param trans monotonic function to apply to the smooth and residuals, before plotting.
#'              Monotonicity is not checked. 
#' @param seWithMean if TRUE the component smooths are shown with confidence intervals that 
#'                   include the uncertainty about the overall mean. If FALSE then the uncertainty
#'                   relates purely to the centred smooth itself. Marra and Wood (2012) suggests 
#'                   that TRUE results in better coverage performance, and this is also suggested 
#'                   by simulation.
#' @param unconditional if \code{TRUE} then the smoothing parameter uncertainty corrected covariance 
#'                      matrix is used to compute uncertainty bands, if available.
#'                      Otherwise the bands treat the smoothing parameters as fixed.
#' @param ... currently unused.
#' @return Returns \code{NULL} invisibly.
#' @references Marra, G and S.N. Wood (2012) Coverage Properties of Confidence Intervals for 
#'             Generalized Additive Model Components. Scandinavian Journal of Statistics.
#' @name plotRGL.mgcv.smooth.MD
#' @examples 
#' # Example 1: taken from ?mgcv::te, shows how tensor pruduct deals nicely with 
#' # badly scaled covariates (range of x 5% of range of z )
#' library(mgcViz)
#' n <- 1e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n)
#' 
#' ob <- (x-z)^2 + (y-z)^2 + rnorm(n)
#' b <- gam(ob ~ s(x, y, z))
#' v <- getViz(b)
#' 
#' plotRGL(sm(v, 1), fix = c("z" = 0))
#' 
#' # Need to load rgl at this point
#' \dontrun{
#' library(rgl)
#' rgl.close() # Close
#' 
#' plotRGL(sm(v, 1), fix = c("z" = 1), residuals = TRUE)
#' 
#' # We can still work on the plot, for instance change the aspect ratio
#' aspect3d(1, 2, 1)
#' 
#' rgl.close() # Close
#' }
#' 
#' @rdname plotRGL.mgcv.smooth.MD
#' @export plotRGL.mgcv.smooth.MD
#' @export
#' 
plotRGL.mgcv.smooth.MD <- function(x, fix, se = TRUE, n = 40, residuals = FALSE, type = "auto", 
                                   maxpo = 1e3, too.far = c(0, NA), xlab = NULL, ylab = NULL, 
                                   main = NULL, xlim = NULL, ylim = NULL, se.mult = 1, 
                                   trans = identity, seWithMean = FALSE, 
                                   unconditional = FALSE, ...){
  
  pack <- requireNamespace("rgl", quietly=TRUE)
  if( !pack ){
    message("Please install the rgl package to use this function.")
    return(NULL)
  }
  
  if (type == "auto") { type <- .getResTypeAndMethod(x$gObj$family)$type }
  if ( length(too.far) == 1 ){ too.far <- c(too.far, NA)  }
  
  P <- .prepareP(o = x, unconditional = unconditional, residuals = residuals, 
                 resDen = "none", se = TRUE, se.mult = se.mult, n = NULL, n2 = n,  
                 xlab = xlab, ylab = ylab, main = main, ylim = ylim, xlim = xlim,
                 too.far = too.far, seWithMean = seWithMean, fix = fix)
  
  R <- list()
  if( residuals ) {
    # NB we are not passing P$xlim or P$ylim here
    R <- .getResidualsPlotRGL(gamObj = x$gObj, X = P$raw, type = type, maxpo = maxpo,
                              xlimit = xlim, ylimit = ylim, exclude = P$exclude2)
    P$raw <- R$raw
  }
  
  # Actual plotting
  P$plotCI <- se
  .plotRGL.mgcv.smooth.2D(P = P, res = R$res, trans = trans)
  
}
