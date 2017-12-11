#' Visualizing 2D smooth effects in 3D 
#' 
#' @description This method plots an interactive 3D representation of a 2D smooth effect, using
#'              the [rgl] package.
#' @param x a smooth effect object, extracted using [mgcViz::sm].
#' @param se when TRUE (default) upper and lower surfaces are added to the plot at \code{se.mult} 
#'           (see below) standard deviations for the fitted surface.
#' @param n sqrt of the number of grid points used to compute the effect plot.
#' @param residuals if TRUE, then the partial residuals will be added.
#' @param type the type of residuals that should be plotted. See [residuals.gamViz].
#' @param maxpo maximum number of residuals points that will be plotted. 
#'              If number of datapoints > \code{maxpo}, then a subsample of \code{maxpo} points will be taken.
#' @param too.far if greater than 0 then this is used to determine when a location is too far 
#'               from data to be plotted. This is useful since smooths tend to go wild 
#'               away from data. The data are scaled into the unit square before deciding
#'               what to exclude, and too.far is a distance within the unit square.
#'               Setting to zero can make plotting faster for large datasets, but care 
#'               then needed with interpretation of plots.
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
#' @name plotRGL.mgcv.smooth.2D
#' @examples 
#' # Example 1: taken from ?mgcv::te, shows how tensor pruduct deals nicely with 
#' # badly scaled covariates (range of x 5% of range of z )
#' library(mgcViz)
#' 
#' # Simulate some data
#' test1 <- function(x,z,sx=0.3,sz=0.4) { 
#'   x <- x*20
#'   (pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2)+
#'                  0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2))
#' }
#' n <- 500
#' old.par <- par(mfrow=c(2,2))
#' x <- runif(n)/20;z <- runif(n);
#' xs <- seq(0,1,length=30)/20;zs <- seq(0,1,length=30)
#' pr <- data.frame(x=rep(xs,30),z=rep(zs,rep(30,30)))
#' truth <- matrix(test1(pr$x,pr$z),30,30)
#' f <- test1(x,z)
#' y <- f + rnorm(n)*0.2
#' 
#' # Fit with t.p.r.s. basis and plot
#' b1 <- gam(y~s(x,z))
#' plotRGL(sm(getViz(b1), 1))
#' 
#' rgl.close() # Close
#' 
#' # Fit with tensor products basis and plot (with residuals)
#' b2 <- gam(y~te(x,z))
#' plotRGL(sm(getViz(b2), 1), residuals = TRUE)
#' 
#' # We can still work on the plot, for instance change the aspect ratio
#' library(rgl)
#' aspect3d(1, 2, 1)
#' 
#' rgl.close() # Close
#' @importFrom rgl .check3d light3d surface3d axes3d title3d spheres3d aspect3d
#' @rdname plotRGL.mgcv.smooth.2D
#' @export plotRGL.mgcv.smooth.2D
#' @export
#' 
plotRGL.mgcv.smooth.2D <- function(x, se = TRUE, n = 40, residuals = FALSE, type = "auto", 
                                   maxpo = 1e3, too.far = 0, xlab = NULL, ylab = NULL, 
                                   main = NULL, xlim = NULL, ylim = NULL,  se.mult = 1, 
                                   trans = identity, seWithMean = FALSE, 
                                   unconditional = FALSE, ...){
  
  if (type == "auto") { type <- .getResTypeAndMethod(x$gObj$family$family)$type }
  
  P <- .prepareP(o = x, unconditional = unconditional, residuals = residuals, 
                 resDen = "none", se = TRUE, se.mult = se.mult, n = NULL, n2 = n,  
                 xlab = xlab, ylab = ylab, main = main, ylim = ylim, xlim = xlim,
                 too.far = too.far, seWithMean = seWithMean)
  
  R <- list()
  if( residuals ) {
    # NB we are not passing P$xlim or P$ylim here
    R <- .getResidualsPlotRGL(gamObj = x$gObj, X = P$raw, type = type, maxpo = maxpo,
                              xlimit = xlim, ylimit = ylim, exclude = P$exclude2)
    P$raw <- R$raw
  }

  # Actual plotting
  P$plotCI <- se
  .plotRGL.mgcv.smooth.2D(P = P, trans = trans, res = R$res)
  
}

##########
# Internal function that gets the residuals and checks that they are within the boundaries
.getResidualsPlotRGL <- function(gamObj, X, type, maxpo, xlimit, ylimit, exclude, trans)
{
  res <- residuals.gamViz(gamObj, type = type) 

  # Checking if we are too far from current slice: relevant only for plotRGL.mgcv.smooth.MD
  if( any(exclude) ){
    res <- res[ !exclude ]
    X <- X[ !exclude, ]
  }
  
  # Boundary checking for residuals: inefficient, but simple approach.
  if( !is.null(xlimit) || !is.null(ylimit) ){
    if( is.null(xlimit) ) xlimit <- c(-Inf, Inf)
    if( is.null(ylimit) ) ylimit <- c(-Inf, Inf)
      rin <- X$x > xlimit[1] & X$x < xlimit[2] & X$y > ylimit[1] & X$y < ylimit[2]
      X <- X[rin, , drop = F]
      res <- res[rin]
  }
  
  # Subsample residuals
  if( length(res) > maxpo){
    ind <- sample(1:length(res), maxpo)
    X <- X[ind, ]
    res <- res[ ind ]
  }
  
  return( list("res" = res, "raw" = X) )
  
}

##########
# Internal function for plotting
.plotRGL.mgcv.smooth.2D <- function(P, trans, res = NULL) {
  
  # New window and setup env
  .check3d()
  
  # Draws non-parametric density
  n <- length(P$x)
  surface3d(P$x, P$y, matrix(P$fit, n, n), color="#FF2222", alpha=0.5)
  if( P$plotCI ){
    surface3d(P$x, P$y, matrix(P$fit + P$se, n, n), 
              alpha=0.5, color="#CCCCFF",front="lines")
    surface3d(P$x, P$y, matrix(P$fit - P$se, n, n), 
              alpha=0.5, color= "#CCCCFF", front="lines")
  }
  
  # Draws the simulated data as spheres on the baseline
  if( !is.null(res) ){
    cent = min(P$fit-3*P$se)
    surface3d(P$x, P$y, matrix(cent, n, n), color="#CCCCFF",
              front = "lines", back = "lines")
    axes3d(c('x', 'y', "z")) 
    title3d(xlab = P$xlab, ylab = P$ylab, main = P$main)
    res <- res / max(abs(res)) * max(P$se)
    spheres3d(P$raw$x, P$raw$y, cent + res, 
              radius=max(c(abs(P$fit), P$x, P$y))/100, 
              color= ifelse(res<0, "red", "blue"))
  } else {
    axes3d(c('x', 'y', "z")) 
    title3d(xlab = P$xlab, ylab = P$ylab, main = P$main)
  }
  
  aspect3d(1, 1, 1)
  
  return( invisible(NULL) )
  
}

