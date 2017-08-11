#' Visualizing 2D slice of smooth effects in 3D (interactively)
#' 
#' @description XXX
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
#' plotRGL(v(1), fix = c("z" = 0))
#' 
#' rgl.close() # Close
#' 
#' plotRGL(v(1), fix = c("z" = 1), residuals = T)
#' 
#' # We can still work on the plot, for instance change the aspect ratio
#' library(rgl)
#' aspect3d(1, 2, 1)
#' 
#' rgl.close() # Close
#' 
#' @rdname plotRGL.mgcv.smooth.MD
#' @export plotRGL.mgcv.smooth.MD
plotRGL.mgcv.smooth.MD <- function(o, fix, se = TRUE, n = 40, residuals = FALSE, type = "auto", 
                                   maxpo = 1e3, too.far = c(0, NA), xlab = NULL, ylab = NULL, 
                                   main = NULL, xlim = NULL, ylim = NULL, se.mult = 1, 
                                   shift = 0, trans = I, seWithMean = FALSE, 
                                   unconditional = FALSE){
  
  if (type == "auto") { type <- mgcViz:::.getResTypeAndMethod(o$gObj$family$family)$type }
  if ( length(too.far) == 1 ){ too.far <- c(too.far, NA)  }
  
  P <- .prepareP(o = o, unconditional = unconditional, residuals = residuals, 
                 resDen = "none", se = se, se.mult = se.mult, n = NULL, n2 = n,  
                 xlab = xlab, ylab = ylab, main = main, ylim = ylim, xlim = xlim,
                 too.far = too.far, seWithMean = seWithMean, fix = fix)
  
  R <- list()
  if( residuals ) {
    # NB we are not passing P$xlim or P$ylim here
    R <- .getResidualsPlotRGL(gamObj = o$gObj, X = P$raw, type = type, maxpo = maxpo,
                              xlimit = xlim, ylimit = ylim, exclude = P$exclude2)
    P$raw <- R$raw
  }
  
  # Actual plotting
  .plotRGL.mgcv.smooth.2D(P = P, res = R$res)
  
}
